#lang racket

;;2zad

(define (count-greater num lst)
  (cond [(empty? lst) 0]
        [(> (first lst) num) (+ 1 (count-greater num (rest lst)))]
        [else                     (count-greater num (rest lst))]))

(define (num-bigger-elements lst)
  (define (helper el)
    (list el (count-greater el lst)))
  
  (map helper lst)
)
