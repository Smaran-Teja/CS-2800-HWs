#lang racket

(require lsl/performance)

;; Problem 1

;; part p1a
(define (min-list l)
  (if (empty? l)
      #f
      (local
        [(define min-list-rest (min-list (rest l)))]
        (if (or (boolean? min-list-rest)
                (< (first l) min-list-rest))
            (first l)
            min-list-rest))))


;(visualize (build-list 15 (lambda (n) (build-list n (lambda (x) 1)))) min-list)

;; part p1a

;; Problem 2

;; part p2a
(define (maybe-second l)
  (if (and (cons? l) (cons? (rest l)))
      (first (rest l))
      #f))
;; part p2a
;(visualize (build-list 12 (lambda (n) (build-list (* n 100000) (lambda (x) 1)))) maybe-second)

;; Problem 3

;; part p3a
(define (list-reverse l)
  (reverse l))


(define (real-rev l accum) (if (empty? l)
                               accum
                               (real-rev (rest l) (cons (first l) accum))))

(define (scam-rev l) (reverse l))


;(visualize (build-list 10 (lambda (n) (build-list (* n 100) (lambda (_) 1)))) list-reverse)
;; part p3a

;; Problem 4

;; part p4a

(define (fib-help n prev cur next) (if (<= n 0)
                                       next
                                       (fib-help (sub1 n) cur (+ cur prev) (+ cur prev))))





(define (fib n)
  (cond [(<= n 1) 1]
        [else (fib-help n 0 1 1)]))

;(visualize (build-list 8 (lambda (n) (* n 5))) fib)
;; part p4a
