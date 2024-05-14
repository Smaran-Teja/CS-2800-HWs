#lang lsl

;; Problem 1


;; part p1a
(define-contract Heap (List Integer))

(: empty-heap Heap)
(define empty-heap empty)
;; part p1a

;; part p1b
(define-contract (Maybe T) (OneOf T (Constant #f)))

(: insert (-> Heap Integer Heap))
(define (insert h n) (cond [(empty? h) (list n)]
                           [(< (first h) n) (cons (first h) (insert (rest h) n))]
                           [else (cons n h)]))

(: get-min (-> Heap (Maybe Integer)))
(define (get-min h) (cond [(empty? h) #f]
                          [(cons? h) (first h)]))

(: minimum (-> (List Integer) Integer))
(define (minimum l) (cond [(empty? l) 9999999999999999999999999999999999999999999]
                          [(cons? l) (if (< (first l) (minimum (rest l)))
                                         (first l)
                                         (minimum (rest l)))]))

(: remove-min (-> Heap Heap))
(define (remove-min h) (if (empty? h)
                           '()
                           (rest h)))





;; part p1b


;; Problem 2

;; part p2a
(: satisfies-heap-invariant? (-> Heap Boolean))
(define (satisfies-heap-invariant? h) (if (empty? h)
                                          #t
                                          (and (= (minimum h) (get-min h)) (satisfies-heap-invariant? (rest h)))))
;; part p2a



;; part p2b
(define-contract CheckedHeap (AllOf Heap satisfies-heap-invariant?))
;; part p2b

;; part p2c
(: insert-checked (-> CheckedHeap Integer CheckedHeap))
(define (insert-checked h n)
  (insert h n))

(: get-min-checked (-> CheckedHeap (Maybe Integer)))
(define (get-min-checked h)
  (get-min h))

(: remove-min-checked (-> CheckedHeap CheckedHeap))
(define (remove-min-checked h)
  (remove-min h))
;; part p2c


;; Problem 3

;; part p3a
(: heap-library (Exists (A) (Tuple A (-> A Integer A) (-> A (Maybe Integer)) (-> A A))))
(define heap-library
  (list empty-heap
        insert
        get-min
        remove-min))
;; part p3a