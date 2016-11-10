#lang racket

;;ok

;Задача 2. Да се дефинира процедура (count-occurrences l1 l2), която по зададени списъци l1 и l2
;връща като резултат броя на срещанията на списъка l2 в списъка l1.

(define (countOccurrences lst query)
  (define (matchInBeginning lst query)
    (cond
      [(< (length lst) (length query)) #F]
      
      [(empty? query) #T]
      
      [else (and
             (= (first lst) (first query))
             (matchInBeginning (rest lst) (rest query)))]
    )
  )
  
  (cond
    [(< (length lst) (length query)) 0]
    [(matchInBeginning lst query) (+ 1 (countOccurrences (rest lst) query))]
    [else                              (countOccurrences (rest lst) query)]
  )
)

(countOccurrences '(1 2 3   2 1 4 1 2 3) '(1 2)) ;; 2
(countOccurrences '(1 2 3 1 2 1 4 1 2 3) '(1 2)) ;; 3
(countOccurrences '(1 2) '(1 2))                 ;; 1
(countOccurrences '(1 2) '(1 2 3 4))             ;; 0
