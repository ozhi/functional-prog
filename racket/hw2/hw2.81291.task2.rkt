#lang racket

(define (convert num from-base to-base)
  
  (define (from-base->dec num)
    (if (< num 10)
        num
        (+ (* from-base (from-base->dec (quotient num 10)))
           (remainder num 10))))

  (define (dec->to-base num)
    (if (<= num 0)
        0
        (+ (* 10 (dec->to-base (quotient num to-base)))
           (remainder num to-base))))
  
  (dec->to-base (from-base->dec num))
)

(convert 123 10 2) ;-> 1111011
(convert 173 8 10) ;-> 123
(convert 173 8 2) ;-> 1111011
(convert 0 8 2) ; ->0
