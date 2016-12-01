#lang racket

(define (derive-n n f eps)
  (define (derive f eps)
    (λ (x)
        (/ (- (f (+ x eps)) (f x)) eps)
    )
  )
  
  (if (= n 0)
      
      f
      
      (derive
       (derive-n (- n 1) f eps)
       eps
      )
  )
)

(define (cube x) (* x x x))

((derive-n 2 cube 0.0000001) 5)