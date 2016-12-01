#lang racket

(define (countDivisors n)
  (define (countDivisorsHelper curNum count)
    (cond
      [(> curNum n) count]
      [(zero? (remainder n curNum)) (countDivisorsHelper (+ curNum 1) (+ count 1))]
      [else (countDivisorsHelper (+ curNum 1) count)]
    )
  )

  (countDivisorsHelper 1 0)
)