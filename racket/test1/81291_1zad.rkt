#lang racket

;;1zad

(define (digs-decr? num)
  (if (< num 10)
      #T
      (and
       (<= (remainder num 10) (remainder (quotient num 10) 10))
       (digs-decr? (quotient num 10)))))

(define (sum-numbers a b)
  (cond
    [(> a b) 0]
    [(digs-decr? a) (+ a (sum-numbers (+ a 1) b))]
    [else                (sum-numbers (+ a 1) b)]))
