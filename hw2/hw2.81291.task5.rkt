#lang racket

(define t1 '(1
             ()
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

(define (subtrees level tree) ;; returns a list of the subtrees of tree of a particular level
  (if (= 0 level)

      (list tree)

      (apply
       append
       (map
        (λ (x) (subtrees (- level 1) x))
        (filter
         (λ (x) (not (empty? x)))
         (rest tree)
        )
       )
      )
  )
)

(define (tolist tree) ;;returns a list of the edges of level 1 of tree
  (map
   (λ (x) (list (first tree) (first x)))
   (filter
    (λ (x) (not (empty? x)))
    (rest tree)
    )
   )
)

(define (edges-on-level tree lvl)
  (apply
   append
   (map
    (λ (x) (tolist x))
    (filter
     (λ (x) (not (empty? x)))
     (subtrees (- lvl 1) tree)
     )
    )
  )
)

(edges-on-level t1 2)
