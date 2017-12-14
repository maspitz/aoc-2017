#lang racket

(require rackunit)

(check-equal? (distance-from-string "ne,ne,ne") 3)
(check-equal? (distance-from-string "ne,ne,sw,sw") 0)
(check-equal? (distance-from-string "ne,ne,s,s") 2)
(check-equal? (distance-from-string "se,sw,se,sw,sw") 3)

(distance-from-file "input.txt")

