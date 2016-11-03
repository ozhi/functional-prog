#lang racket

;;not tested

(define (minelem l)
  (define (helper l curMin)
    (cond [(= l '()) curMin]
          [(< (first l) curMin) (helper (rest l) (first l))]
          [else                 (helper (rest l) curMin))]
  )
  
  (helper l (first l))
)