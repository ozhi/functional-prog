#lang racket

(define (neighbors node edges) ;;could be made shorter with assq?
  
  (let* ([node-and-neighbors (filter
                              (λ (x) (= node (first x)))
                              edges)]
         
         [node-and-neighbors-length (length node-and-neighbors)])
    
    (cond [(zero? node-and-neighbors-length) '()]
          [(< 1 node-and-neighbors-length) (error "ERR: node mentioned more than once in edges")]
          
          [(= 1 (length (first node-and-neighbors))) '()]
          [else (rest (first node-and-neighbors))]))
)

(define (remove-node node edges)
  (let*
      ([removed-neighbors-of-node (filter
                                   (λ (x) (not (= node (first x))))
                                   edges)]
       
       [removed-node-as-neighbor (map
                                  (λ (x)
                                    (filter
                                     (λ (x) (not (= node x)))
                                     x))
                                  removed-neighbors-of-node)]
       
       [removed-neighborless-nodes (filter
                                    (λ (x) (> (length x) 1))
                                    removed-node-as-neighbor)])
    
    removed-neighborless-nodes)
)

(define (longest-el lst) ;;lst must be a list of lists; this returns the longest element of lst
  (cond
    [(empty? lst)              (error "ERR: Empty list passed to longst-el")]
    [(not (list? (first lst))) (error "ERR: Invalid arguments passed to longest-el")]
    
    [(= 1 (length lst)) (first lst)]
    
    [(> (length (longest-el (rest lst))) (length (first lst)))
     (longest-el (rest lst))] ;;recursive call done twice, could be fixed with let
    
    [else (first lst)])
)

(define (max-way edges from to) ;; if a way exists => returns it as a list, otherwise => returns #F
  (cond [(= from to) (list from)]
    
        [(empty? (neighbors from edges)) #F]
        
        [else (let* ([all-finishes (map
                                    (λ (x) (max-way (remove-node from edges) x to))
                                    (neighbors from edges))]
                     
                     [non-false-finishes (filter
                                          (λ (x) (not (equal? x #F)))
                                          all-finishes)])
                
                (if (null? non-false-finishes)
                    #F
                    (cons from (longest-el non-false-finishes))))])
)

(define (max-cycle edges node) ;;much of the body of max-way is copied here
  (cond [(empty? (neighbors node edges)) #F] ;;if no such cycle exists, return #F
        
        [else (let* ([all-finishes (map
                                    (λ (x) (max-way edges x node)) ;;here lies the difference between max-way and max-cycle
                                    (neighbors node edges))]
                     
                     [non-false-finishes (filter
                                          (λ (x) (not (equal? x #F)))
                                          all-finishes)])
                
                (if (null? non-false-finishes)
                    #F
                    (cons node (longest-el non-false-finishes))))])
)


(define e1 '((1 2) (2 3) (3 1 4) (4 2) (7)))
(define e2 '((1 2 3 4 5 6) (2 1 3 4 5 6) (3 1 2 4 5 6) (4 1 2 3 5 6) (5 1 2 3 4 6) (6 1 2 3 4 5)))
(define e4 '((1 2 3 4) (2 3) (3 5) (4 5 6) (5 7) (6 7) (7 4)))

;(remove-node 2 e1)
;(neighbors 7 e1)

;(equal? (max-way e1 1 1) '(1))
;(equal? (max-way e1 1 2) '(1 2))
;(equal? (max-way e1 1 3) '(1 2 3))
;(equal? (max-way e1 1 4) '(1 2 3 4))
;(equal? (max-way e1 2 1) '(2 3 1))
;(equal? (max-way e1 2 2) '(2))
;(equal? (max-way e1 2 3) '(2 3))
;(equal? (max-way e1 2 4) '(2 3 4))
;(equal? (max-way e1 3 1) '(3 1))
;(equal? (max-way e1 3 2) '(3 1 2)) ;; '(3 4 2) would also be a correct result
;(equal? (max-way e1 3 3) '(3))
;(equal? (max-way e1 3 4) '(3 4))
;(equal? (max-way e1 4 1) '(4 2 3 1))
;(equal? (max-way e1 4 2) '(4 2))
;(equal? (max-way e1 4 3) '(4 2 3))
;(equal? (max-way e1 4 4) '(4))

;(max-way e4 3 6) ;; '(3 5 7 4 6)

(max-cycle e2 4)
(max-cycle e1 1)
