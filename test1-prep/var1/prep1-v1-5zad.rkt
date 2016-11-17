#lang racket

;;ok

;;Задача 5. Да се дефинира функция (all-sums lst), която получава списък lst от не повече от 10
;цели неотрицателни числа и връща нов списък, съдържащ сумите на числата във всички
;възможни непразни подмножества на lst, но без повторения и в какъвто и да е ред.
;;Пример 1: Нека имаме списък с числата 1, 2, 3. Всички възможни суми за подмножества са: 1 = 1,
;2 = 2, 3 = 1+2 = 3, 4 = 1+3, 5 = 2+3 и 6 = 1+2+3.
;Пример 2: Нека имаме списък с числата 0, 1, 2. Всички възможни суми за подмножества са: 0 = 0,
;1 = 1 = 0+1, 2 = 2 = 0+2, 3 = 1+2 = 0+1+2.

(define (all-sums lst)

  (define (all-sums-raw lst)
    (cond
      [(empty? lst) '()]
      
      [(= 1 (length lst)) (list 0 (first lst))]
      
      [else (let ([all-sums-rest (all-sums-raw (rest lst))])
              (append
               all-sums-rest
               (map
                (λ (x) (+ x (first lst)))
                all-sums-rest))
              )]))

  (define (remove-duplicates lst)
    (if (empty? lst)
        '()
        
        (append
         (list (first lst))
         (filter
          (λ (x) (not (= x (first lst))))
          (remove-duplicates (rest lst)))) ))

  (define (remove-zeros lst)
    (filter
     (λ (x) (not (zero? x)))
     lst))

  (define (contains-zero? lst)
    (not (empty? (filter zero? lst))))
  
  (define ans (remove-duplicates (remove-zeros (all-sums-raw lst))))
  
  (if (contains-zero? lst)  
      (cons 0 ans)
      ans)
)

(all-sums (list 1 2 3)) ;; → '(1 2 3 4 5 6)
(all-sums (list 0 1 2)) ;; → '(0 1 2 3)