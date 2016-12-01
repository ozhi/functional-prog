#lang racket

;;works

(define (sumSquaresOfOddNumbersIn l)
  (cond [(null? l) 0]
        [(zero? (remainder (first l) 2)) (sumSquaresOfOddNumbersIn (rest l))]
        [else                            (+ (sqr (first l)) (sumSquaresOfOddNumbersIn (rest l)))]
  )
)

(define (better l)
  (apply + (map sqr (filter odd? l)))
)

(sumSquaresOfOddNumbersIn (list 1 2 3 4 5))
(better (list 1 2 3 4 5))