#lang racket

(define (sumFirstNPrimesGreaterThanK n k)

  (define (prime? n)
    (define (primeHelper div)
      (cond
        [(> (* div div) n)          #T]
        [(zero? (remainder n div))  #F]
        [else                       (primeHelper (+ div 1))]
      )
    )
    
    (if (< n 2)
        #F
        (primeHelper 2)
    )
  )

  (define (helper curNum numsSummed sum) ;;recursion simulating iteration
    (cond
      [(>= numsSummed n)
       sum]
      [(and (> curNum k) (prime? curNum))
       (helper (+ curNum 1) (+ numsSummed 1) (+ sum curNum))]
      [else
       (helper (+ curNum 1) numsSummed sum)] 
    )
  )

  (helper 2 0 0)
)
