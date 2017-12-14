#lang racket

;;; see http://adventofcode.com/2017/day/13

(require rackunit)

;; triple coordinate of hex.
;; coordinates not independent: nw + ne + s = 0.
(struct hex (nw s ne) #:transparent)

(define (hex-add h1 h2)
  (hex (+ (hex-nw h1) (hex-nw h2))
       (+ (hex-s h1) (hex-s h2))
       (+ (hex-ne h1) (hex-ne h2))))

(define (string->hex s)
  (case s
    [("nw") (hex 1 0 0)]
    [("s") (hex 0 1 0)]
    [("ne") (hex 0 0 1)]
    [("se") (hex -1 0 0)]
    [("n") (hex 0 -1 0)]
    [("sw") (hex 0 0 -1)]))

(define (distance-from-hex h)
  (define a (hex-nw h))
  (define b (hex-ne h))
  (define c (hex-s h))
  ;; A coord in the form
  ;;   a*ne + b*nw + c*s 
  ;; specifies a path of length |a|+|b|+|c| from origin to the
  ;; point specified by that coord.
  ;;
  ;; We can always add or subtract (ne + nw + s = 0) to find
  ;; an alternative coord and path to the same point.
  ;;
  ;; The minimal path will have at least one of a,b,c = 0.
  ;; So construct three candidate lengths and take the smallest.
  (define (cand-path x y z) ;; zeroing out the third coord
    (+ (abs (- x z)) (abs (- y z))))
  (min (cand-path a b c)
       (cand-path b c a)
       (cand-path c a b)))

(define (distance-from-string s)
  (define string-list (regexp-split #rx"," s))
  (define hex-list (map string->hex string-list))
  (define final-hex (foldl hex-add (hex 0 0 0) hex-list))
  (distance-from-hex final-hex))

(define (distance-from-file path)
  (with-input-from-file path
    (lambda ()
      (distance-from-string (read-line)))))

;;;;; tests for part 1

(check-equal? (distance-from-string "ne,ne,ne") 3)
(check-equal? (distance-from-string "ne,ne,sw,sw") 0)
(check-equal? (distance-from-string "ne,ne,s,s") 2)
(check-equal? (distance-from-string "se,sw,se,sw,sw") 3)

;;;;; answer for part 1

(distance-from-file "input.txt")



(define ((running-sum op) x lst)
  (cons (op x (car lst)) lst))

(define (distances-from-file path)
  (with-input-from-file path
    (lambda ()
      (define string-list (regexp-split #rx"," (read-line)))
      (define step-list (map string->hex string-list))
      (define position-list
        (foldl (running-sum hex-add) `(,(hex 0 0 0)) step-list))
      (map distance-from-hex position-list)
)))

(define max-distance 
  (foldl max 0 (distances-from-file "input.txt")))

;;;;; answer for part 2

max-distance

