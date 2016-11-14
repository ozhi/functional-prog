#lang racket

(define (sum-numbers str)
  (define (digit? c)
    (and (>= (char->integer c) 48)
         (<= (char->integer c) 57)))
  
  (define (char->digit c)
    (if (digit? c)
        (- (char->integer c) 48)
        (error "ERR: invalid argument passed to char->digit")))
  
  
  (define (helper lst curNum sum)
    (cond
      [(null? lst) (+ sum curNum)]
      
      [(digit? (car lst)) (helper
                           (cdr lst)
                           (+
                            (char->digit (car lst))
                            (* 10 curNum))
                           sum)]
      
      [else (helper (cdr lst) 0 (+ sum curNum))]
      )
    )
  
  (helper (string->list str) 0 0)
)

(sum-numbers "a123b2c56") ;; → 181
(sum-numbers "a1b2c3") ;; → 6
(sum-numbers "000abracadabra") ;; → 0

