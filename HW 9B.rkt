#lang lsl

;; Problem 1

;; part p1a
(define-struct counter (val))

(: make-counter-1 (-> (-> Natural Natural)))
(define make-counter-1
  (let ([c (make-counter 0)])
    (lambda ()
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))

(: make-counter-2 (-> (-> Natural Natural)))
(define make-counter-2
  (lambda ()
    (let ([c (make-counter 0)])
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))
;; part p1a

;; part p1b
(: counter-distinguish (-> (-> (-> Natural Natural)) Natural))

(define (counter-distinguish mk-cntr) (begin
                                          ((mk-cntr) 5)
                                          ((mk-cntr) 10)))

;(counter-distinguish make-counter-2)

(check-expect (not (equal? (counter-distinguish make-counter-1)
                           (counter-distinguish make-counter-2)))
              #t)
;; part p1b

;; Problem 2
;; part p2a
(: fast-incr (-> (Counter Natural) (Counter Natural) Natural))
(define (fast-incr c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))
;; part p2a

;; part p2b
(: fast-incr-prop (-> (Counter Natural) (Counter Natural) True))
(define (fast-incr-prop c1 c2)
  (equal? (+ (counter-val c1) (counter-val c2) 2)
          (fast-incr c1 c2)))
;; part p2b


;; part p2c

(: fast-incr-exercise (-> Natural))
(define (fast-incr-exercise) (local
                               [(define cntr (make-counter 5))]
                               (fast-incr cntr cntr)))

;; part p2c


;; Problem 3

;; part p3a
(: fast-incr-fixed (Function (arguments [c1 (Counter Natural)] [c2 (Counter Natural)])
                             (result (AllOf Natural
                                            (lambda (x) (not (eq? c1 c2)))))))
(define (fast-incr-fixed c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))
;; part p3a

;; Problem 4

;; part p4a
(define-struct mcons (first rest))
(define-contract MList (OneOf empty? (Mcons Integer MList)))
;; part p4a

;; part p4b

(define (unique? l) (cond [(empty? l) #t]
                          [else (and (not (member? (first l) (rest l)))
                                     (unique? (rest l)))]))
(define-contract UniqueList unique?)
(: ids UniqueList) 
(define ids empty)


(: mlength (Function (arguments [l (AllOf (Record ids))])
                     (result Natural)))
(define (mlength ml)
  (cond [(empty? ml) 0]
        [(mcons? ml) (add1 (mlength (mcons-rest ml)))]))


(define A (make-mcons 1 (make-mcons 2 empty)))
(set-mcons-rest! A A)

;; part p4b