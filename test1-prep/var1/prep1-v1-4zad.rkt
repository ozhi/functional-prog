#lang racket

;;ok

;;Задача 4. Да се дефинира функция (pair-compose fs), която получава списък
;(f1 f2 f3 ... fn) от едноаргументни числови функции и връща нова едноаргументна числова
;функция g - такава, че оценката на (g x) е равна на сумата
;(f1 . f2) (x) + (f3 . f4) (x) + ... + (fn-1 . fn) (x), където “.” означава композиция на функции.
;Ако оригиналният списък с функции има нечетен брой елементи, то последната функция от
;списъка се композира с функцията идентитет, която получава един аргумент и го връща без
;промяна.

(define (pair-compose fs)
  (cond
    [(= 0 (length fs)) (λ (x) x)]
    
    [(= 1 (length fs)) (first fs)]
    
    [(= 2 (length fs))
     (λ (x) ((first fs) ((second fs) x)))]
    
    [else
     (λ (x)
       (+
        ((first fs) ((second fs) x))
        ((pair-compose (rest (rest fs))) x)))])
)

(define (add1 x) (+ x 1))
(define (add2 x) (+ x 2))
(define (mult2 x) (* x 2))

((pair-compose (list add1 add2)) 1) ;; -> 4

;;(pair-compose (list add1 mult2 mult2 add2))
((pair-compose (list add1 mult2 mult2 add2)) 1) ;;(+ (add1 (mult2 1)) (mult2 (add2 1))) -> (+ 3 6) -> 9
((pair-compose (list add1 mult2 mult2 add2 add2)) 1) ;; -> 12
