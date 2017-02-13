#lang racket
(require "fusion.rkt")

(define N 20000000)

(time
 (void (fuse (foldl + 0 (filter even? (range N))))))

(time
 (void (foldl + 0 (filter even? (range N)))))
