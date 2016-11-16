#lang racket

;;ok

;;Задача 1. Да се дефинира функция (sum-sum-digit a b k), която намира сумата на естествените
;;числа от a до b (0<a≤b), сумата от цифрите на които е кратна на k.

(define (sum-sum-digit a b k)
  (define (sum-dig x)
    (if (positive? x)
        
        (+
         (remainder x 10)
         (sum-dig (quotient x 10)))
        
        0))
  
  (cond
    [(> a b) 0]
    
    [(zero? (remainder (sum-dig a) k))
     (+ a (sum-sum-digit (+ a 1) b k))]
    
    [else (sum-sum-digit (+ a 1) b k)])
  )

(sum-sum-digit 10 20 27) ;;0
(sum-sum-digit 16 16 8)  ;;0
(sum-sum-digit 16 16 7)  ;;16
(sum-sum-digit 1 100 1)  ;;5050
