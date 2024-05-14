#lang lsl

;; Problem 1
(: exclusive-range? (-> Integer Integer Integer Boolean))
(define (exclusive-range? lo hi n) (cond [(and (> n lo) (< n hi)) #t]
                                         [else #f]))
(check-contract exclusive-range?)

(: exclusive-range?-prop (-> Integer Integer True))
(define (exclusive-range?-prop n1 n2) (local
                                        [(define lo (if (< n1 n2) n1 n2))
                                         (define hi (if (<= n1 n2) n2 n1))]
                                        (if (< (abs (- n2 n1)) 2) 
                                            #t
                                            (exclusive-range? lo hi (random (add1 lo) hi)))))

(check-contract exclusive-range?-prop 100000)

;; Problem 2
(define-contract Odd (Immediate (check (lambda (x) (and (integer? x) (not (even? x)))))
                                (generate (lambda (fuel) (+ (* 2 (contract-generate Integer fuel)) 1)))))


(: double-plus1 (-> Odd Odd))
(define (double-plus1 x) (+ (* 2 x) 1))

(check-contract double-plus1)
;; Problem 3


(: divisible-by-3-or-5? (-> Integer Boolean))
(define (divisible-by-3-or-5? n) (if (or (= (modulo n 3) 0) (= (modulo n 5) 0))
                                     #t
                                     #f))

(define-contract Divis3or5 (Immediate (check divisible-by-3-or-5?)
                                      (generate (lambda (fuel) (local [(define div3? (contract-generate Boolean))
                                                                       (define rand (if div3?
                                                                                           (* 3 (random (add1 fuel)))
                                                                                           (* 5 (random (add1 fuel)))))]
                                                                 rand)))))

(: divide-3-or-5 (-> Integer Divis3or5))
(define (divide-3-or-5 x) (if (or (= (modulo x 3) 0) (= (modulo x 5) 0))
                              x
                              0))

(check-contract divide-3-or-5)


