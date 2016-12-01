#lang racket

(define (maximize lst)

  ;;по условие взимаме функцията, със стойност най-голяма по модул,
  ;;но я връщаме със знака и, а не по модул 
  (define (abs-max a b)
    (if (> (abs a) (abs b)) a b))
  
  (if (empty? (rest lst))
      (first lst)
      
      (λ (x) (let* ([maximize-rest (maximize (rest lst))]
                    [result-first ((first lst) x)]
                    [result-maximize-rest (maximize-rest x)])
               
               (abs-max result-first result-maximize-rest ))))
)            

((maximize (list
            (λ (x) (- x 10))
            (λ (x) (- x 5)))) 5) ;;-> -5 ;;а не 5, както е в условието

((maximize (list
            (λ (x) (- x 10))
            (λ (x) (- x 5)))) 9) ;;-> 4

((maximize (list (λ (x) (- x 10))
                 (λ (x) (- x 11))
                 (λ (x) (- x 12))
                 (λ (x) (- x 13))
                 (λ (x) (- x 14))
                 (λ (x) (- x 15))
                 (λ (x) (- x 16))
                 (λ (x) (- x 17)))) 10) ;; -> -7
