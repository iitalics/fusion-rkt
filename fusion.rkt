#lang racket

(begin-for-syntax
  (require racket
           racket/syntax
           syntax/parse
           syntax/id-table)

  (struct fuse (ph? pattern)
    #:transparent)

  (define (fuse-name fu)
    (syntax-case (fuse-pattern fu)
        ()
      [(f _ ...) (syntax->datum #'f)]
      [#f '<default>]
      [f (syntax->datum #'f)]))

  (define placeholder
    (make-parameter #'_?_))

  (struct consumer fuse
    (dir init-gen fold-gen)
    #:transparent)

  ;; patn : syntax?
  ;; dir : (or/c 'left 'right)
  ;; (init-gen args) -> syntax?
  ;; (fold-gen args) -> syntax?
  (define (make-consumer patn dir #:init init-gen #:fold fold-gen)
    (consumer #t patn dir init-gen fold-gen))

  (struct producer fuse
    (left-gen right-gen)
    #:transparent)

  ;; patn : syntax?
  ;; (right-gen [f/init : syntax?] [f/next : syntax?] args) -> syntax?
  ;; (left-gen [f/init : syntax?] [f/next : syntax?] args) -> syntax?
  (define (make-producer patn #:left left-gen #:right right-gen)
    (producer #f patn left-gen right-gen))

  (struct transformer fuse
    (fold-gen)
    #:transparent)

  ;; patn : syntax?
  ;; (fold-gen [f/next : syntax?] args) -> syntax?
  (define (make-transformer patn fold-gen)
    (transformer #t patn fold-gen))

  ;;; Default consumer (when no consumer found; produces list).
  (define co:default
    (make-consumer #f
                   'right
                   #:init (lambda (_) #''())
                   #:fold (lambda (_) #'cons)))

  ;;; Default producer (when no producer found; expects a list).
  (define pr:default
    (make-producer #f
                   #:left (lambda (f/i f/n a)
                            (with-syntax ([a a] [z f/i] [f f/n])
                              #'(foldl f z a)))
                   #:right (lambda (f/i f/n a)
                             (with-syntax ([a a] [z f/i] [f f/n])
                               #'(foldr f z a)))))

  ;;; Table of fuse patterns
  (define fuse-patterns
    (make-free-id-table))

  ;;; Add a pattern
  (define (add-fuse-pattern! id fu)
    (free-id-table-update! fuse-patterns id
                           (lambda (lst) (cons fu lst))
                           (lambda () '())))

  (add-fuse-pattern!
   #'length
   (make-consumer #'(length _?_) 'left
                  #:init (lambda (_) #'0)
                  #:fold (lambda (_) #'(lambda (_ n) (add1 n)))
                  ))

  (add-fuse-pattern!
   #'foldl
   (make-consumer #'(foldl f z _?_) 'left
                  #:init (lambda (args)
                           (with-syntax ([(f z) args])
                             #'z))
                  #:fold (lambda (args)
                           (with-syntax ([(f z) args])
                             #'f))
                  ))

  (add-fuse-pattern!
   #'reverse
   (make-consumer #'(reverse _?_) 'left
                  #:init (lambda (_) #''())
                  #:fold (lambda (_) #'cons)))

  (add-fuse-pattern!
   #'range
   (make-producer #'(range n)
                  #:left (lambda (f/i f/n a)
                           (with-syntax ([(n) a]
                                         [init f/i]
                                         [next f/n])
                             #'(let it ([i 0]
                                        [acc init])
                                 (if (= i n) acc
                                     (it (add1 i) (next i acc))))))
                  #:right (lambda (f/i f/n a)
                            (with-syntax ([(n) a]
                                          [init f/i]
                                          [next f/n])
                              #'(let it ([i (sub1 n)]
                                         [acc init])
                                  (if (negative? i) acc
                                      (it (sub1 i) (next i acc))))))
                  ))

  (add-fuse-pattern!
   #'map
   (make-transformer #'(map f _?_)
                     (lambda (f/n a)
                       (with-syntax ([(f) a]
                                     [next f/n])
                         #'(lambda (x z) (next (f x) z))))
                     ))

  (add-fuse-pattern!
   #'filter
   (make-transformer #'(filter f _?_)
                     (lambda (f/n a)
                       (with-syntax ([(f) a]
                                     [next f/n])
                         #'(lambda (x z)
                             (if (f x) (next x z) z))))
                     ))

  ;; (parse-pattern stx handler) -> any
  ;; stx : syntax?
  (define (parse-pattern stx)
    (let ([patn-name
           (syntax-case stx ()
             [(fn args ...) #'fn]
             [id #'id])])
      (match
          (for/first ([fu (in-list (free-id-table-ref fuse-patterns
                                                      patn-name
                                                      '()))]
                      #:when (pattern-matches? (fuse-pattern fu) stx))
            (let-values ([(args ph)
                          (pattern-deconstruct (fuse-pattern fu)
                                               stx
                                               (fuse-ph? fu))])
              (list fu args ph)))
        ;
        [#f (values #f '() stx)]
        [(list fu args ph) (values fu args ph)])))

  ;;; Does the syntax match the pattern?
  (define (pattern-matches? patn in)
    (syntax-case patn ()
      [(_ args ...)
       (syntax-case in ()
         [(_ in-args ...)
          (= (length (syntax->list #'(in-args ...)))
             (length (syntax->list #'(args ...))))]
         [_ #f])]
      [_
       (symbol? (syntax-e in))]))

  ;;; Deconstruct pattern
  ;; (pattern-deconstruct patn in ph?)
  ;;   -> list? syntax?
  (define (pattern-deconstruct patn in ph?)
    (syntax-case patn ()
      [(_ args ...)
       (syntax-case in ()
         [(_ in-args ...)
          (let ([ph #f])
            ; extract arguments & placeholder argument
            (let ([args (foldr (lambda (arg in-arg acc)
                                 (cond
                                   [(and ph? (free-identifier=? arg (placeholder)))
                                    (set! ph in-arg) acc]
                                   [else
                                    (cons in-arg acc)]))
                               '()
                               (syntax->list #'(args ...))
                               (syntax->list #'(in-args ...)))])
              (when (and ph? (false? ph))
                (error "no placeholder symbol found in pattern!"))
              (values args ph)))])]
      [_ (values '() in)]))

  ;;; PARSE FUSE TRASNFORMERS
  (define (parse-fuse-transformers stx)
    (define pr-pair #f)
    (define (parse-steps stx)
      (let-values ([(fu args ph) (parse-pattern stx)])
        (match fu
          [(? transformer? tr)
           (cons (cons tr args)
                 (parse-steps ph))]
          [(? producer? pr)
           (set! pr-pair (cons pr args))
           '()]
          [_
           (printf "; default\n  stx = ~a\n  ph = ~a\n"
                   (syntax->datum stx)
                   (syntax->datum ph))
           (set! pr-pair (cons pr:default stx))
           '()])))
    ;
    (values (parse-steps stx)
            pr-pair))

  ;;;; PARSE FUSE HERE
  (define (parse-fuse stx co-pair)
    (define-values (tr-pairs pr-pair)
      (parse-fuse-transformers stx))
    (gen-fuse pr-pair
              tr-pairs
              co-pair))

  ;;; GENERATE FUSE CODE
  (define (gen-fuse pr-pair  ; producer
                    tr-pairs ; transformers
                    co-pair) ; consumer

    ; generate code for producer
    (define (gen-pr dir pr args init-src next-src)
      (let ([gen (case dir
                   [(left) (producer-left-gen pr)]
                   [(right) (producer-right-gen pr)])])
        (gen init-src
             next-src
             args)))

    ; generate code for transformers (calls body-gen at end)
    (define (gen-trs prev-fold
                     tr-pairs
                     body-gen)
      (match tr-pairs
        [(cons (cons tr args) tr-rest)
         (let ([fold-gen (transformer-fold-gen tr)]
               [new-var (gensym)])
           (with-syntax ([fold-src (fold-gen prev-fold args)]
                         [var new-var]
                         [body (gen-trs new-var
                                        tr-rest
                                        body-gen)])
             #'(let ([var fold-src])
                 body)))]
        ['()
         (body-gen prev-fold)]))

    ; generate code for consumer
    (define (gen-co co args)
      (let* ([co-init-gen (consumer-init-gen co)]
             [co-fold-gen (consumer-fold-gen co)]
             [init-src (co-init-gen args)]
             [fold-src (co-fold-gen args)]
             [first-fold-var (gensym)]
             [body (gen-trs first-fold-var
                            tr-pairs
                            (lambda (last-var)
                              (gen-pr (consumer-dir co)
                                      (car pr-pair)
                                      (cdr pr-pair)
                                      init-src
                                      last-var)))])
        (with-syntax ([var first-fold-var]
                      [fold-src fold-src]
                      [body body])
          #'(let ([var fold-src])
              body))))

    ; put it all together
    (with-syntax ([res
                   (gen-co (car co-pair)
                           (cdr co-pair))])
      #'res))
  )


;;; THE FUSE MACRO STARTS HERE
;;; But most of it happens in parse-fuse and gen-fuse
(provide fuse)
(define-syntax (fuse stx)
  (syntax-case stx ()
    [(_ form)
     (let-values ([(fu args ph) (parse-pattern #'form)])
       (match fu
         [(? consumer? co)
          (parse-fuse ph (cons co args))]
         [(? transformer?)
          (parse-fuse #'form (cons co:default '()))]
         [(? producer?)
          (printf "encountered producer; nothing to be done.\n")
          #'form]
         [#f
          (printf "no matching found. perhaps nest fuse deeper?")
          #'form]))]))


;;; A cool macro
;;; (unflat (map string-length ..)
;;;         (string-split .. ", ")
;;;         "first, second, third")
;;;   becomes
;;; (map string-length (string-split "first, second, third" ", "))
(provide unflat)
(define-syntax (unflat stx)
  (syntax-case stx ()
    [(_ (form ...) forms ... last-form)
     (datum->syntax #'(form ...)
                    (map (lambda (e) (syntax-case e (..)
                                  [.. #'(unflat forms ... last-form)]
                                  [_ e]))
                         (syntax->list #'(form ...))))]
    [(_ last-form)
     #'last-form]))
