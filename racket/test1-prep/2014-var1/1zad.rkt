#lang racket

;;ok

;;Да се напише предикат (truncatable-prime? x), който връща "истина" точно когато едно число х
;притежава едновременно следните свойства:
;числото х е просто
;всички числа, които се получават чрез премахване на цифри в края на х също са прости
;Пример за такова число е 3797, тъй като 3797 е просто и числата, които се получават чрез
;последователно премахване на цифри в края му (379, 37 и 3), също са прости.

(define (truncatable-prime? x)

  (define (prime? x)
    (define (helper div)
      (cond
        [(> (* div div) x) #T]
        [(zero? (remainder x div)) #F]
        [else (helper (+ div 1))]))
    (if (< x 2) #F (helper 2)))

  (cond
    [(zero? x) #T]
    [(not (prime? x)) #F]
    [else (truncatable-prime? (quotient x 10))])
)


(truncatable-prime? 3797) ;; #T
(truncatable-prime? 37)  ;; #T
(truncatable-prime? 3)   ;; #T
(truncatable-prime? 17)  ;; #F
(truncatable-prime? 44)  ;; #F
(truncatable-prime? 1)   ;; #F
