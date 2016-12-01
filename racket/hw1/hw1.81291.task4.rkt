#lang racket
  
(define (gcd-logarithmic a b)
  (if (zero? b)
      a
      (gcd-logarithmic b (remainder a b))
  )
)

(define (gcd-linear a b) ;;my official solution
  (cond [(= a b) a]
        [(> a b) (gcd-linear (- a b) b)]
        [(< a b) (gcd-linear a (- b a))]))

(define (gcd-nonIterative a b) ;;a linear non iterative solution
  
  (define (max a b) (if (> a b) a b))
  
  (define (helper a b div)
    (cond
      [(> div (max a b))
       1]
      
      [(and (zero? (remainder a div)) (zero? (remainder b div)))
       (* div (helper (quotient a div) (quotient b div) div))]
      
      [else
       (helper a b (+ div 1))]
    )
  )

  (helper a b  2)
)

(define gcd gcd-linear)
