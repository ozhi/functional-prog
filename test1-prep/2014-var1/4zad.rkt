#lang racket

;;ok

;;Да се напише функция (magic-square? M), която проверява дали квадратната матрица M е магически квадрат.
;Магически квадрат е квадратна матрица, в която всички редове, стълбове, главният и вторият диагонал имат
;една и съща сума.

(define (get-diag1 matrix)
      (if (empty? matrix)
          '()
          (cons (first (first matrix)) (get-diag1 (rest (map rest matrix))))))

(define (last-el lst)
  (if (empty? (rest lst))
      (first lst)
      (last-el (rest lst))))

(define (all-but-last-el lst)
  (cond
    [(empty? (rest lst)) '()]
    [(empty? (rest (rest lst))) (list (first lst))]
    [else (cons (first lst) (all-but-last-el (rest lst)))]))

(define (get-diag2 matrix)
      (if (empty? matrix)
          '()
          (cons (last-el (first matrix)) (get-diag2 (rest (map all-but-last-el matrix))))))


(define (magic-square? matrix)
  (define sum (apply + (first matrix))) ;; we assume no empty matrix is passed to the func

  (define (check-rows sum matrix)
    (if (empty? matrix)
        #T
        (and
         (= sum (apply + (first matrix)))
         (check-rows sum (rest matrix)))))

  (define (check-cols sum matrix)
    (if (empty? (first matrix))
        #T
        (and
         (= sum (apply + (map first matrix)))
         (check-cols sum (map rest matrix)))))
  
  (define (check-diag1 sum matrix)   
    (= sum (apply + (get-diag1 matrix))))
  

  (and
   (check-rows sum matrix)
   (check-cols sum matrix)
   (= sum (apply + (get-diag1 matrix)))
   (= sum (apply + (get-diag2 matrix))))
)

(magic-square? '((23 28 21)
                 (22 24 26)
                 (27 20 25))) ;; #T

(magic-square? '((1 2 3)
                 (4 5 6)
                 (7 8 9))) ;; #F
