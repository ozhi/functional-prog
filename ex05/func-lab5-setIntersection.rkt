#lang racket

(define (intersection A B)
  (filter (λ (x) (member x A)) B)
)

(intersection '(1 2 3 4) '(1 3 5 9))

(define (union a b)
  (append a (filter (λ (x) (not (member x a))) b))
)

(union '(1 2 3 4) '(2 3 4 5))