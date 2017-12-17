#lang at-exp racket

(require rackunit)

(define (parse-data in)
  (define d (make-hash))
  (for/hash ([line (in-lines in)])
    (define matches (regexp-match #rx"([0-9]*): ([0-9]*)" line))
    (define layer-depth (string->number (cadr matches)))
    (define layer-range (string->number (caddr matches)))
    (values layer-depth layer-range)))

(define (trip-severity data)
  (for/sum ([(ldepth lrange) (in-hash data)]
            #:when (= 0 (modulo ldepth (* 2 (- lrange 1)))))
    (* ldepth lrange)))

(define test-input @~a{0: 3
                       1: 2
                       4: 4
                       6: 4})

(define test-data (parse-data (open-input-string test-input)))

(check-equal? (trip-severity test-data) 24)

(define input-data (call-with-input-file "input.txt" parse-data))
(trip-severity input-data)

(define (trip-caught? delay-time data)
  (for/or ([(ldepth lrange) (in-hash data)])
    (= 0 (modulo (+ delay-time ldepth) (* 2 (- lrange 1))))))

(define max-delay  9999999)

(define (find-not-caught data)
  (define not-caught -1)
  (for ([delay (in-range 9999999)]
        #:break (not (trip-caught? delay data)))
    (set! not-caught delay))
  (add1 not-caught)
)

(check-equal? (find-not-caught test-data) 10)

(find-not-caught input-data)


           
