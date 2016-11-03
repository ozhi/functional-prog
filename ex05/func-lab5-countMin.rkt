#lang racket

(define (countMin l) ;;returns how many time the minimal element of a list is found in it
  (let* (
         [minEl (apply min l)]
         [equalsMin (λ (x) (= x minEl))]
         [listOfMins (filter equalsMin l)]
         [answer (length listOfMins)])
    answer
  )
)

(countMin '(4 5 2 6 2 4 6 3 2))
