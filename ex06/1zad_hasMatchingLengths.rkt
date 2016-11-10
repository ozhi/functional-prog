#lang racket

;;ok

;Задача 1. Да се дефинира предикат (hasMatchingLengths l1 l2). l1 и l2 са непразни списъци от спи-
;съци от числа. Ако l1 = (a1 a2 ... ak), а l2 = (b1 b2 ... bk), предикатът да връща истина, когато разли-
;ките в дължините на всички двойки съответните списъци ai и bi са еднакви.

(define (hasMatchingLengths l1 l2)
  (define (getDifferences l1 l2)
    (map - (map length  l1) (map length l2)) ;; outer map takes two list arguments!
  )
  
  (define (diffEqual? x lst)
    (cond
      [(null? lst) #T]
      [(not (= x (first lst))) #F]
      [else (diffEqual? x (rest lst))]
    )
  )
  
  (diffEqual?
   (first (getDifferences l1 l2))
   (rest (getDifferences l1 l2)))
)

(define l1 '((1 2 3)   (1 2 3 4)   ()  ()  (1 2)))
(define l2 '((1 2 3 4) (1 2 3 4 5) (1) (1) (1 2 3)))
(define l3 '((1 2 3)   (1 2 3)     ()  ()  (1 2)))

(hasMatchingLengths l1 l2) ;;#T
(hasMatchingLengths l1 l3) ;;#F
