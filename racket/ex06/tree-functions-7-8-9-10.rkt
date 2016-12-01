#lang racket

;;ok

;tree-functions + zad 7,8,9,10

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

'_______TEST_Tree_Functions___OK___
(define t (make-tree
           1
           (make-tree
            2
            empty-tree
            empty-tree)
           (make-tree
            3
            empty-tree
            (make-tree
             7
             empty-tree
             empty-tree))))

t
(root t)
(left-tree t)
(right-tree t)
(left-tree (right-tree t))
(root (left-tree (right-tree t))) ;; #F
(leaf? (right-tree (right-tree t))) ;; #T

;; 7zad - ok
'_______7_ZAD___OK___
(define (tree->list tree)
  (if (empty-tree? tree)
      '()
      (append
       (tree->list (left-tree tree))
       (list (root tree))
       (tree->list (right-tree tree))
       )
   ))

(tree->list t)

;;8zad - ok

'_______8_ZAD___OK___
(define (level tree k)
  (cond
    [(empty-tree? tree)
     '()]

    [(zero? k)
     (list (root tree))]

    [else
     (append
      (level (left-tree tree) (- k 1))
      (level (right-tree tree) (- k 1)))]
  )
)

(level t 0)
(level t 1)
(level t 2)
(level t 3)
(level t 19)

;;9zad - ok
'_______9_ZAD___OK___

(define (max-elem tree)
  (if (empty-tree? tree)
      #F
      
      (let*
          ([empty-left (empty-tree? (left-tree tree))]
           [empty-right (empty-tree? (right-tree tree))])
        
        (cond
          [(and empty-left empty-right) (root tree)]
          
          [empty-left (max
                       (root tree)
                       (max-elem (right-tree tree)))]
          
          [empty-right (max
                        (root tree)
                        (max-elem (left-tree tree)))]
          
          [else (max
                 (root tree)
                 (max-elem (left-tree tree))
                 (max-elem (right-tree tree)))]
        ))
  )
)

(max-elem t)
(max-elem (left-tree t))
(max-elem (right-tree t))
(max-elem (left-tree (right-tree t)))


;;10zad - ok
'_______10_ZAD___OK___

(define (add x bst) ;;binary search tree
  (cond
    [(empty-tree? bst) (make-tree x empty-tree empty-tree)]

    [(= x (root bst)) bst] ;;no duplicates in our BST

    [(< x (root bst)) (make-tree
                       (root bst)
                       (add x (left-tree bst))
                       (right-tree bst))]
    [else (make-tree
           (root bst)
           (left-tree bst)
           (add x (right-tree bst)))]
  )
)

(define (member? x bst)
  (cond
    [(empty? bst) #F]
    [(< x (root bst)) (member? x (left-tree bst))]
    [(> x (root bst)) (member? x (right-tree bst))]
    [else #T]))

(define bst
  (add 63
     (add 8
          (add 7
               (add 6
                    (add 0
                         (add 2
                              (add 1
                                   (add 5
                                        (add 13
                                             (add 87
                                                  (add 40
                                                       (add 10
                                                            (add 90
                                                                 (add 30
                                                                      (add 85
                                                                           (add 20
                                                                                (add 25
                                                                                     (add 50 empty-tree)))))))))))))))))))

bst
(member? 25 bst) ;; #T
(member? 50 bst) ;; #T
(member? 30 bst) ;; #T
(member? 31 bst) ;; #F
