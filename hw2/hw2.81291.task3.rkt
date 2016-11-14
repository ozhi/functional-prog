#lang racket

(define (maximize lst)

  (define (abs-max a b)
    (if (> (abs a) (abs b)) a b))
  
  (if (= 1 (length lst))

      (first lst)

      (λ (x)
        (let*
            ([maximize-rest (maximize (rest lst))]
             [result-first ((first lst) x)]
             [result-maximize-rest (maximize-rest x)])

          (abs-max result-first result-maximize-rest )
        )
      )
  )
)            

((maximize (list
            (λ (x) (- x 10))
            (λ (x) (- x 5))))
 5) ;;-> -5

((maximize (list
            (λ (x) (- x 10))
            (λ (x) (- x 5))))
 9) ;;-> 4
