#lang racket

(require rackunit)

(check-equal? (distance-from-string "ne,ne,ne") 3)
(check-equal? (distance-from-string "ne,ne,sw,sw") 0)
(check-equal? (distance-from-string "ne,ne,s,s") 2)
(check-equal? (distance-from-string "se,sw,se,sw,sw") 3)

(distance-from-file "input.txt")

(define (distance-from-file path)
  (with-input-from-file path
    (lambda ()
      (distance-from-string (read-string)))))

;; triple coordinate of hex.
;; coordinates not independent: nw + ne = n + n.
(struct hex (nw n ne) #:transparent)

;;; a path is a list of steps such as '("ne" "s" "sw") etc...
(define (path-from-string s)
  (regexp-split #rx"," s))

(define (hex-from-path p)
  (define h (make-hash))
  (for ([step (in-list p)])
    (hash-set! h step (add1 (hash-ref h step 0))))
  (hex (- (hash-ref h "nw" 0) (hash-ref h "se" 0))
       (- (hash-ref h "n" 0) (hash-ref h "s" 0))
       (- (hash-ref h "ne" 0) (hash-ref h "sw" 0))))

(define (distance-from-hex)
  (define (canonical-hex h)
    ()))

(define (distance-from-string s)
  (distance-from-hex (hex-from-path (path-from-string s))))
