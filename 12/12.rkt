#lang racket

(require rackunit)


(define (add-edge! h node new-dest)
  (define prev-dests (hash-ref h node '()))
  (hash-set! h node (cons new-dest prev-dests)))

(define (add-edges-from-line! h str)
  (define m (regexp-match #rx"([0-9]*) <-> (.*)" str))
  (define node (string->number (cadr m)))
  (define dest-list (regexp-split #rx", " (caddr m)))
  (for ([dest (in-list dest-list)])
    (add-edge! h node (string->number dest))))

;; return list of nodes visited in a depth first search
(define (find-component-nodes edge-list node)
  (define visited (make-hash))
  (define (visit! node)
    (unless (hash-ref visited node #f)
      (hash-set! visited node #t)
      (for ([dest (in-list (hash-ref edge-list node '()))])
        (visit! dest))))
  (visit! node)
  (hash-keys visited))

(define test-edges (make-hash))

(define test-lines '("0 <-> 2"
                     "1 <-> 1"
                     "2 <-> 0, 3, 4"
                     "3 <-> 2, 4"
                     "4 <-> 2, 3, 6"
                     "5 <-> 6"
                     "6 <-> 4, 5"))

(for ([l (in-list test-lines)])
  (add-edges-from-line! test-edges l))

(check-equal?  (sort (find-component-nodes test-edges 0) <) '(0 2 3 4 5 6))

(define edges (make-hash))

;;; find part 1 answer

(define input-file "input.txt")

(define (add-edges-from-port! in)
  (for ([l (in-lines in)])
    (add-edges-from-line! edges l)))

(call-with-input-file input-file add-edges-from-port!)

;;; answer to part 1

(length (find-component-nodes edges 0))

(define nodes-in-components (make-hash))

(define (components edge-list)
  (for/list ([n (in-hash-keys edge-list)]
             #:unless (hash-ref nodes-in-components n #f))
    (begin
      (define cmp (find-component-nodes edge-list n))
      (for ([c (in-list cmp)])
        (hash-set! nodes-in-components c #t))
      cmp)))

(length (components edges))
