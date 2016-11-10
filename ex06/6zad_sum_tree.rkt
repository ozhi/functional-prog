#lang racket

;;not ok

;Задача 6. Да се дефинира процедура (sum-tree tree), която намира сумата от върховете на дадено
;двоично дърво tree от реални числа.

(define (make-tree root left-tree right-tree)
  (list root left-tree right-tree))

(define (make-empty-tree) '())

(define (empty-tree? tree)
  (empty? tree) ;;empty? is the same as null?
)

(define (root tree)
  (first tree))

(define (left-tree tree)
  (first (rest tree)))

(define (right-tree tree)
  (rest (rest tree)))

(define (leaf? tree)
  (and
   (empty-tree? (left-tree tree))
   (empty-tree? (right-tree tree)))
)
  
(define (sum-tree tree)
  (if (empty-tree? tree)
      0
      (+ (root tree)
         (sum-tree (left-tree tree))
         (sum-tree (right-tree tree)))
  )
)
  
(define t1 '(1 (2 () ()) (3 () (7 () ()))))
(sum-tree t1)
  
  
  