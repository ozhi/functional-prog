#lang racket

;;not tested

;Задача 5. Да се дефинират конструктор (make-tree root left-tree right-tree),
;предикати (empty-tree? tree) и (leaf? tree) и селектори (root tree),
;(left-tree tree) и (right-tree tree) за работа с двоични дървета.

(define (make-tree root left-tree right-tree)
  (list root left-tree right-tree)
)

(define (make-empty-tree) '())

(define (empty-tree? tree)
  (empty? tree) ;;empty? is the same as null?
)

(define (root tree)
  (first tree))

(define (left-tree tree)
  (first (rest tree)))

(define (right-tree tree)
  (rest (rest tree))

(define (leaf? tree)
  (and
   (empty-tree? (left-tree tree))
   (empty-tree? (right-tree tree)))
)

;;example
(define tree (make-tree
              1
              (make-tree 2 (make-empty-tree) (make-empty-tree))
              (make-tree
               3
               (make-empty-tree)
               (make-tree 7 (make-empty-tree) (make-empty-tree)))))
  