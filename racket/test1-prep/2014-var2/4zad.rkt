#lang racket

;; ok

;;Напишете функция (all-permutations? items), която получава като аргумент
;списък от естествени числа items и връща #t, ако десетичните записи на всички числа от items са
;пермутации на една и съща поредица от цифри, или #f в противния случай.

(define (add-val-to-pos pos val lst) ;; ok
  (if (= 0 pos)
      (cons (+ (first lst) val) (rest lst))
      (cons (first lst) (add-val-to-pos (- pos 1) val (rest lst)))))
;(define l '(0 1 2 3 4 5 6 7 8 9))
;(add-val-to-pos 4 96 l)
  
(define (number->diglist num) ;; ok
  (define (helper num lst)
    (if (zero? num)
        lst
        (helper
         (quotient num 10)
         (add-val-to-pos (remainder num 10) 1 lst))))

  (helper num '(0 0 0 0 0 0 0 0 0 0))
)
;(number->diglist 900)

(define (equal-lists? l1 l2) ;;guaranteed to be of same length
  (if (empty? l1)
      #T
      (and
       (= (first l1) (first l2))
       (equal-lists? (rest l1) (rest l2)))))

(define (all-same-elements? lst) ;; ok
  (cond [(empty? lst) #T]
        [(empty? (rest lst)) #T]
        [else (and
               (equal-lists? (first lst) (second lst))
               (all-same-elements? (rest lst)))]))
;(all-same-elements '(1 1 1 1))
;(all-same-elements '(1 3 1 2))
;(all-same-elements '(1))
;(all-same-elements '())

(define (all-permutations? items)
  (all-same-elements? (map number->diglist items)))

(all-permutations? (list 123 321 231)) ; резултат #t
(all-permutations? (list 12345 54321 32514 12453)) ; резултат #t
(all-permutations? (list 12345 54321 32514 12353)) ; резултат #f