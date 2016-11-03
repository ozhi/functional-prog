#lang racket

;;works

(define (composition l)
  (if (= 1 (length l))
      (first l)
      (λ (x)
         ((first l) ((composition (rest l)) x))
      )
  )
)

(
 (composition (list
              (λ (x) (+ x 1))
              (λ (x) (+ x 2))
              (λ (x) (* x x))
              ))
 2
)