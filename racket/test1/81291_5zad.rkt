#lang racket

;;5zad

(define o '((1 2 3)
            (4 5 6)
            (7 8 9)))

(define (get-i-j i j matrix) ;; ok
  ;;matrix is indexed from 1
  
  (cond [(< i 1) 0]
        [(< j 1) 0]

        [(> i (length matrix)) 0]
        [(> j (length (first matrix))) 0]

        [(and (= 1 i) (= 1 j)) (first (first matrix))]
        [(= 1 i) (get-i-j i (- j 1) (map rest matrix))]

        [else (get-i-j (- i 1) j (rest matrix))])
)

(define (sum-nei i j matrix) (+ (get-i-j (- i 1) (- j 1) matrix) ;; ok
                                (get-i-j (- i 1)    j    matrix)
                                (get-i-j (- i 1) (+ j 1) matrix)
   
                                (get-i-j    i    (- j 1) matrix)
                                (get-i-j    i    (+ j 1) matrix)
   
                                (get-i-j (+ i 1) (- j 1) matrix)
                                (get-i-j (+ i 1)    j    matrix)
                                (get-i-j (+ i 1) (+ j 1) matrix)))

(define (calc-new-val old-val sum-nei)
  (cond [(< sum-nei 2) 0]
        [(> sum-nei 3) 0]
        [(= sum-nei 3) 1]
        [else old-val]))

(define (change-list pos lst val) ;; no argument validation ;; ok
  (if (= 1 pos)
      (cons val (rest lst))
      (cons (first lst) (change-list (- pos 1) (rest lst) val))))

(define (change-i-j i j matrix val) ;; no argument validation ;; ok
  (if (= 1 i)
      (cons
       (change-list j (first matrix) val)
       (rest matrix))
    
      (cons
       (first matrix)
       (change-i-j (- i 1) j (rest matrix) val))))

(define (game-of-life m)
  (define (helper i j result)
    (cond  [(> i (length m)) result]
           [(> j (length (first m))) (helper (+ 1 i) 1 result)]
           [else (helper i (+ j 1) (change-i-j i j result (calc-new-val
                                                           (get-i-j i j m)
                                                           (sum-nei i j m))))]))
  (helper 1 1 m)
)






(define block '((0 0 0 0)
                (0 1 1 0)
                (0 1 1 0)
                (0 0 0 0)))

(define blinker '((0 1 0)
                  (0 1 0)
                  (0 1 0)))

(define single-cell '((0 0 0)
                      (0 1 0)
                      (0 0 0)))

(define last-man-standing '((1 0 0)
                            (0 1 0)
                            (0 0 1)))

(game-of-life block) ; '((0 0 0 0) (0 1 1 0) (0 1 1 0) (0 0 0 0))
(game-of-life blinker) ; '((0 0 0) (1 1 1) (0 0 0))
(game-of-life single-cell) ; '((0 0 0) (0 0 0) (0 0 0))
(game-of-life last-man-standing) ; '((0 0 0)(0 1 0)(0 0 0))

