#lang racket

;;?

;Задача 4. Да се напише функция (zero matrix), която получава като аргумент матрица matrix и
;връща като резултат нова матрица, в която всеки стълб на matrix, съдържащ нула, се състои само
;от нули.

(define (transp matrix) ;;???
  (if (null? (first matrix))
      '()
      (append (list (map first matrix)) (transp (map rest matrix)))
      )
  )

(define (transp2 matrix) ;;???
  (apply map list matrix)
)

(define (zero2 matrix)

  
  (define (processRow row)
    (if (equal? #F (member 0 row))
        row
        (map (λ (x) 0) row)
    )              
  )
  
  (transp (map processRow (transp matrix))) 
)

(zero2 (list
       (list 1 2 0)
       (list 3 4 1)
       (list 0 5 7)
       (list 4 2 4))) ; ‘((0 0 0) (3 4 1) (0 0 0) (4 2 4))

(transp2 (list
       (list 1 2 0)
       (list 3 4 1)
       (list 0 5 7)
       (list 4 2 4))) ; ‘((0 0 0) (3 4 1) (0 0 0) (4 2 4))
