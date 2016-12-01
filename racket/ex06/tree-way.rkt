#lang racket

;; ok

;;Given an arbitary (non-ordered) binary tree with unique elements,
;;find a way between two given nodes

(define (make-tree root left-tree right-tree)
  (list root left-tree right-tree))

(define empty-tree '())

(define (empty-tree? tree)
  (empty? tree)) ;;empty? is the same as null?

(define (root tree)
  (if (empty-tree? tree)
      #F
      (car  tree)))

(define (left-tree  tree)
  (if (empty-tree? tree)
      #F
      (cadr tree)))

(define (right-tree tree)
  (if (empty-tree? tree)
      #F
      (caddr tree)))

(define (leaf? tree)
  (and
   (empty-tree? (left-tree tree))
   (empty-tree? (right-tree tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t '(1
            (2
             (4
              (8 () ())
              (9 () ()))
             (5
              (10 () ())
              (11 () ())))
            (3
             (6
              (12 () ())
              (13 () ()))
             (7
              (14 () ())
              (15 () ()))))) ;;full binary tree

(define (lvl x tree)
  (cond [(empty-tree? tree) #F] 
        [(= x (root tree)) 0]
        
        [else (let ([left  (lvl x (left-tree  tree))]
                    [right (lvl x (right-tree tree))])
                (cond [left  (+ 1 left)]
                      [right (+ 1 right)]
                      [else #F]))])
)

(define (dad x tree)
  (cond [(empty-tree? tree) #F]
        [(leaf? tree) #F]
        
        [(= x (root (left-tree tree)))  (root tree)]
        [(= x (root (right-tree tree))) (root tree)]
        
        [else (let ([left  (dad x (left-tree  tree))]
                    [right (dad x (right-tree tree))])
                (cond [left left]
                      [right right]
                      [else #F]))]))

(define (tree-way from to tree)
  (let ([lvl-from (lvl from tree)]
        [lvl-to   (lvl to   tree)])
   
    (cond [(= from to) (list from)]
          
          [(>= lvl-from lvl-to)
           (cons from (tree-way (dad from tree) to tree))]
          
          [(< lvl-from lvl-to)
           (append (tree-way from (dad to tree) tree) (list to))]))
)

(tree-way 5 5 t)
(tree-way 5 3 t)
(tree-way 14 15 t)
(tree-way 14 1 t)
