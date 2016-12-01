#lang racket

(define (scons x ll) ;;adds x as a first el of every list that is an element of ll
  (if (null? ll)
      
      null
      
      (cons
       (cons x (first ll))
       (scons x (rest ll))
      )
  )
)

(define (better x ll)
  (map (λ (l) (cons x l)) ll)
)

(better 7 (list
          (list 1 2 3)
          (list 4 5)
          (list 6 7 8)
          (list 9)
          ))