#lang racket

;;ok

;;Задача 3. Да се дефинира предикат (triangular? mat), който получава квадратна числова матрица,
;;представена като списък от списъци, и проверява дали тя е горно триъгълна, т.е. дали всичките
;;елементи под главния й диагонал са нули.

(define (triangular? matrix)

  (define (all-but-first-zeros-old? lst)
    (cond
      [(< (length lst) 2) #T]
      [(not (zero? (second lst))) #F]
      [else (all-but-first-zeros? (cons 0 (rest (rest lst))))]))
  
  (define (all-but-first-zeros? lst) ;;better alternative
    (=
     (length (rest lst))
     (length (filter zero? (rest lst)))))
  
  (cond
    [(empty? matrix) #T]
    [else (and
           (all-but-first-zeros? (map first matrix))
           (triangular? (map rest (rest matrix))))])
)

(triangular? '((1 2 3) (0 5 6) (0 0 9))) ;; → #t
(triangular? '((0 2 3) (0 0 6) (1 0 0))) ;; → #f
