#lang racket

(define (countPalindromesInRange a b) ;;range [a;b] where 0<=a<b, whole numbers

  (define (palindrome n) ;;for the purpose of the main problem this returns 1<=>isPalindrome, 0 otherwise
    (define (reverse n)
      (define (reverseHelper n result)
        (if (positive? n)
            (reverseHelper (quotient n 10) (+ (* result 10) (remainder n 10)))
            result
        )
      )
      
      (reverseHelper n 0)
    )
    
    (if (= n (reverse n))
        1
        0
    )
  )
  
  (define (helper from to count)
    (if (> from to)
        count
        (helper (+ from 1) to (+ count (palindrome from)))
    )
  )

  (helper a b 0)
)