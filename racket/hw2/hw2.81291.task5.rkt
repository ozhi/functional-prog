#lang racket

(define (not-empty-tree? tree) (not (empty? tree)))

(define (subtrees level tree) ;; returns a list of the subtrees of tree of a particular level
  (if (= 0 level)
      (list tree)

      (apply append (map
                     (λ (x) (subtrees (- level 1) x))
                     (filter not-empty-tree? (rest tree)))))
)

(define (tree->root-edges tree) ;;gets a tree, returns '((root child1) (root child2))
  (map
   (λ (x) (list (first tree) (first x)))
   (filter not-empty-tree? (rest tree)))
)

(define (edges-on-level tree lvl) ;;tree - binary tree ;;lvl - natural number
  (apply append (map tree->root-edges
                     (filter not-empty-tree?
                             (subtrees (- lvl 1) tree))))
)

(define t1 '(1
             (2
              (4
               ()
               ())
              (5
               ()
               (7
                ()
                ())))
             (3
              (6
               (8
                ()
                ())
               (9
                ()
                ()))
              ())))

(edges-on-level t1 1)
(edges-on-level t1 2)
(edges-on-level t1 3)
(edges-on-level t1 4)
(edges-on-level t1 35)
