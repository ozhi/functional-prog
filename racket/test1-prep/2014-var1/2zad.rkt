#lang racket

;;ok

;;Да се напише функция (where list-elements list-predicates),
;която връща списък от всички елементи на list-elements,
;за които са изпълнени всички предикати в list-predicates.

(define (where list-el list-pred)

  (define (all-preds-true? x list-pred)
    (if (empty? list-pred)
        #T
        (and
         ((first list-pred) x)
         (all-preds-true? x (rest list-pred)))))

  (filter
   (λ (x) (all-preds-true? x list-pred))
   list-el)
)

(where (list 3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5))))
;резултат (6 8 10)  (списък от всички елементи на дадения, които са  по-големи от 5 и са четни числа)

(where (list 3 4 5 7) (list even? (lambda (x) (> x 5))))
;резултат ( )  (в списъка няма четни числа, по-големи от 5)
