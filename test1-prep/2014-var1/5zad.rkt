#lang racket

;;ok

;;Напишете функция (repeater str), която получава като аргумент символен низ
;и връща анонимна функция на два аргумента - count и glue (число и низ). Оценката на
;обръщението към върнатата функция е низ, който се получава чрез count-кратно
;повтаряне на низа str, при което между всеки две съседни повторения на str стои низът glue.

(define (repeater str)
  (define (repeat count glue)
    (if (zero? count)
        ""
        (string-append str glue (repeat (- count 1) glue))))
  repeat)

((repeater "I love Racket") 3 " ") ; резултат "I love Scheme I love Scheme I love Scheme"
((repeater "Quack") 5 "!") ; резултат "Quack!Quack!Quack!Quack!Quack"
