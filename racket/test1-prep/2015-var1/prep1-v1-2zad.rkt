#lang racket

;;ok

;;Задача 2. Да се дефинира функция (max-ordered-sublist lst), която намира най-дългия възходящо
;;сортиран подсписък на списъка от числа lst.

(define (max-ordered-sublist lst)
  
  (define (max-incr-prefix lst)
    (cond
      [(< (length lst) 2)
       lst]
      
      [(< (first lst) (second lst))
       (append (list (first lst)) (max-incr-prefix (rest lst)))]
      
      [else (list (first lst))]))
  
  (define (longer lst1 lst2)
    (if (> (length lst1) (length lst2)) lst1 lst2))
  
  (if (empty? lst)
      '()
      
      (longer
       (max-incr-prefix lst)
       (max-ordered-sublist (rest lst)))))

(max-ordered-sublist
 '(100 101 50 51 52 53 20 21 1)) ;; → ‘(50 51 52 53)

(max-ordered-sublist
 '(10 11 12 1 2 3 4 5)) ;; → ‘(1 2 3 4 5)

(max-ordered-sublist
 '(1 5 2 4 6 8 3 4 1)) ;; → ‘(2 4 6 8)
