#lang racket

;; not fully tested
;; ok?

;;Напишете функция (cycle times items), която получава като аргументи естествено число times и
;непразен списък items и връща функция на един аргумент - неотрицателно цяло число.
;При оценяване на обръщение към върнатата функция с аргумент n се извършват следните действия:
;Списъкът items се завърта times на брой пъти, като правилото на завъртането е, че
;последният елемент на списъка става пръв и измества всички останали с една позиция надясно.
;Напрмер ако завъртим (1 2 3) един път, получаваме (3 1 2).
;Като оценка на обръщението се връща индексът на първото срещане на n в завъртяния списък.
;Индексирането започва от 0. Ако не се намери елемент на завъртяния списък, съвпадащ с n,
;оценката на обръщението е -1.


(define (cycle times items)

  (define (last-el lst)
    (cond
      [(empty? lst) #F]
      [(empty? (rest lst)) (first lst)]
      [else (last-el (rest lst))]))

  (define (all-but-last-el lst)
    (cond
      [(empty? lst) #F]
      [(empty? (rest lst)) '()]
      [else (cons (first lst) (all-but-last-el (rest lst)))]))
  

  (define (shift-once lst)
    (cons (last-el lst) (all-but-last-el lst)))

  (define (shift times lst)
    (if (zero? times)
        lst
        (shift (- times 1) (shift-once lst))))

  (define (find n lst)
    (cond [(empty? lst) #F]
          [(= n (first lst)) 0]
          [(find n (rest lst)) (+ 1 (find n (rest lst)))]
          [else #F]))
  
  (define cycled-items (shift times items))
  
  (λ (n)
    (if (find n cycled-items)
        (find n cycled-items)
        -1))
)

((cycle 3 (list 1 2 3 4)) 4) ;; 2
((cycle 3 (list 1 2 3 4)) 11) ;; -1
