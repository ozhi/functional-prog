#lang racket

;;ok

;;alternative solution in ex06, using matrix transposition

;;Да се напише функция (zero matrix), която получава като аргумент матрица matrix
;и връща като резултат нова матрица, в която всеки стълб на matrix, съдържащ нула,
;се състои само от нули.

(define (contains-zero? lst)
  (if (empty? lst)
      #F
      (or
       (zero? (first lst))
       (contains-zero? (rest lst)))))

(define (empty-matrix? matrix)
  (zero? (length (first matrix))))

(define (zero matrix)
  (cond
    [(empty-matrix? matrix) matrix]

    [(contains-zero? (map first matrix))
     (map
      (λ (x) (cons 0 x))
      (zero (map rest matrix)))]

    [else
     (map
      cons
      (map first matrix);(λ (x) (cons 7 x))
      (zero (map rest matrix)))])
)

(contains-zero? '(1 2 3 3 4)) ;;#F
(contains-zero? '(1 2 3 0 4)) ;;#T
(contains-zero? '(0))         ;;#T


(define matrix '((1 2 3 0 4)
                 (5 0 6 7 8)
                 (9 1 2 0 3)
                 (0 4 5 6 7)))

(zero matrix)
