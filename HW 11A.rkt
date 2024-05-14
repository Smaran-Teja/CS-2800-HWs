#lang lsl

;; part p0
(define-struct cell (free? value))
(define-contract Cell~ (Cell Boolean Any))

(define-struct memory (pos cells))
(define-contract Memory~ (Memory Natural (List Cell~)))

(define MEMORYSIZE 3)

(define MEMORY
  (make-memory 0 (build-list MEMORYSIZE (lambda (_) (make-cell #t 0)))))


;(define-contract cells (List Cell~))

(: ALLOC-LIST (AllOf (List Cell~)))
(define ALLOC-LIST empty)

(define (add-cell list cell) (cond [(boolean? cell) (if (= (length ALLOC-LIST) MEMORYSIZE)
                                                        list
                                                        (cons -1 list))]
                                   [(member-eq? list cell) (cons -1 list)]
                                   [else (cons cell list)]))

(define (member-eq? list cell) (cond [(empty? list) #f]
                                     [(cons? list) (if (eq? (first list) cell)
                                                       #t
                                                       (member-eq? (rest list) cell))]))
 
(define (remove-cell list cell) (cond [(empty? list) '()]
                                      [(eq? (first list) cell) (rest list)]
                                      [else  (cons (first list)
                                                   (remove-cell (rest list) cell))]))


;; part p0

;; Problem 1

;; part p1
(: malloc (-> (AllOf (Maybe Cell~) (Record add-cell ALLOC-LIST))))
(define (malloc)
  (if (< (memory-pos MEMORY) MEMORYSIZE)
      (local [(define pos (memory-pos MEMORY))
              (define cells (memory-cells MEMORY))
              (define empty-cell (list-ref cells pos))]
        (begin (set-cell-free?! empty-cell #f)
               (set-memory-pos! MEMORY (add1 pos))
               empty-cell))
      (search-all (memory-cells MEMORY))))


;Searches all cells and returns the first free cell it finds, else #f if no free cells
(: search-all (-> (List Cell~) (Maybe Cell~)))
(define (search-all mem-cells) (cond [(empty? mem-cells) #f]
                                     [(cons? mem-cells) (if (cell-free? (first mem-cells))
                                                            (begin (set-cell-free?! (first mem-cells) #f)
                                                                   (first mem-cells))
                                                            (search-all (rest mem-cells)))]))
;; part p1

;; Problem 2

;; part p2
(: free (-> (AllOf Cell~ (Record remove-cell ALLOC-LIST)) False))
(define (free c)
  (begin (set-cell-free?! c #t)
         #f))
;; part p2

;; Problem 3

;; part p3
(: defrag (-> False))
(define (defrag) (local [(define cells (memory-cells MEMORY))
                         (define free-cells (filter (lambda (x) (cell-free? x))
                                                    (memory-cells MEMORY)))
                         (define occupied-cells (filter (lambda (x) (not (cell-free? x)))
                                                        (memory-cells MEMORY)))]
                   
                   (begin (set-memory-cells! MEMORY (append occupied-cells
                                                            free-cells))
                          (set-memory-pos! MEMORY (length occupied-cells))
                          #f)))



#|

(malloc)
(malloc)
(malloc)
(malloc)

(defrag)

(memory)

|#

;; part p3


;; Problem 4

;; part p4a
(define (*= c v)
  (set-cell-value! c v))

(define (deref c) (cell-value c))

(: for-loop (-> Cell~ Natural (-> Any) False))
(define (for-loop idx bound body)
  (if (>= (cell-value idx) bound)
      #f
      (begin (body)
             (set-cell-value! idx (add1 (cell-value idx)))
             (for-loop idx bound body))))
;; part p4a


;; part p4b
(: fib (-> Natural (Maybe Natural)))
(define (fib n) (local
    [(define prev (malloc))
     (define cur (malloc))
     (define next (malloc))
     (define idx (malloc))
     (define (body) (local [(define curVal (cell-value cur))
                            (define newVal (+ (cell-value prev) (cell-value cur)))]
                      (begin (set-cell-value! next newVal)
                             (set-cell-value! prev curVal)
                             (set-cell-value! cur (cell-value next))
                             )))]
    (if (or (equal? prev #f) (equal? cur #f) (equal? next #f) (equal? idx #f))
        #f
        (begin
          (set-cell-value! prev 0)
          (set-cell-value! cur 1)
          (set-cell-value! next 1)
          (set-cell-value! idx 0)
          
          (for-loop idx n body)

          (define ans (cell-value next))
          
          (free prev)
          (free cur)
          (free next)
          (free idx)
          
          ans))))


#|
ALLOC-LIST
(define fst (malloc))
fst
ALLOC-LIST
(define second-last (malloc))
second-last
ALLOC-LIST
(define last (malloc))
last
ALLOC-LIST
(malloc)

"--------------------------"
ALLOC-LIST
MEMORY

(free fst)
"--------------------------"
ALLOC-LIST
MEMORY

(defrag)
"--------------------------"
ALLOC-LIST
MEMORY
|#
;; part p4b


;; Problem 5
;;
;; Add contracts above, no code down here.