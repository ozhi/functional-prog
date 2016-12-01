#lang racket

;;4zad

(define (~~ a b)
  (map
   (λ (func-list arg1-list)
     (map
      (λ (func arg1)
        (λ (x) (func arg1 x))
      )
      func-list
      arg1-list)
   )
   b
   a)
)

(define (<<~ c val)
  (map
   (λ (func-list)
     (map
      (λ (func) (func val))
      func-list)
   )
   c)
)
