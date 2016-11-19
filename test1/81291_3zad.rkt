#lang racket

;;3zad

(define (switchsum f g n)
  (λ (x)
    (if (= 0 n)
        0
        (+ (f x) ((switchsum g f (- n 1)) (f x))))))
