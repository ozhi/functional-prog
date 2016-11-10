#lang racket

;;ok

;Задача 3. Да се напише функция (zero matrix), която получава като аргумент матрица matrix и
;връща като резултат нова матрица, в която всеки ред на matrix, съдържащ нула, се състои само от
;нули.

(define (zero matrix)
  (define (processRow row)
    (if (equal? #F (member 0 row))
        row
        (map (λ (x) 0) row)
    )              
  )
  
  (map processRow matrix) 
)

(zero (list
       (list 1 2 0)
       (list 3 4 1)
       (list 0 5 7)
       (list 4 2 4))) ; ‘((0 0 0) (3 4 1) (0 0 0) (4 2 4))
