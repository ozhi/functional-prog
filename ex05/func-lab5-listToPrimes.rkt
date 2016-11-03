#lang racket

;;Задача 9. Да се дефинира процедура (list2primes l), която по даден списък l от естествени
;;числа, по-големи от 3, получава нов списък от точкови двойки, съответстващи на четните
;;числа в дадения списък и състоящи се от две прости числа, чиято сума е равна на съответ-
;;ното четно число.

(define (listToPrimes l)

  (define (prime? x)
    (define (primeHelper div)
      (cond
        [(> (* div div) x) #T]
        [(zero? (remainder x div)) #F]
        [else (primeHelper (+ div 1))]
      )
    )

    (primeHelper 2)
  )
  
  (define (toPrimeSum x)
    (define (toPrimeSumHelper a b)
      (cond
        [(< a 2) (error "toPrimeSum failed. (bad input?)")]
        
        [(and (prime? a) (prime? b))
         (cons a b)]

        [else (toPrimeSumHelper (- a 1) (+ b 1))]
      )
    )

    (toPrimeSumHelper (- x 2) 2)
  )
  
  (map toPrimeSum (filter even? l))
)

(listToPrimes '(4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))
