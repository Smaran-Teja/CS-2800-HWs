#lang lsl

;; Problem 1
(define-contract PosInt (Immediate (check (lambda (x) (and (integer? x) (> x 0))))
                                   (generate (lambda (fuel) (random 1 (add1 fuel))))))

(: duplicate-element-prop (-> (List Natural) True))
(define (duplicate-element-prop lopi) (local [(define out (duplicate-element lopi))
                                              (define (count num lon) (cond [(empty? lon) 0]
                                                                            [(cons? lon) (if (= (first lon) num)
                                                                                             (add1 (count num (rest lon)))
                                                                                             (count num (rest lon)))]))]
                                             (or (>= (count out lopi) 2) (= out -1))))

(: duplicate-element (-> (List Natural) Integer))
;Purpose: Given a list of positive integers, if there exists at least one that appears more than once,
;         returns one of those repeated integers.
;         If there are no duplicates, returns -1 
(define (duplicate-element lopi) (local [(define sorted (sort lopi <))
                                         (define (duplicate-element-help lopi curNum count)
                                           (cond [(= count 2) curNum] 
                                                 [(empty? lopi) -1]
                                                 [(cons? lopi) (if (= (first lopi) curNum)
                                                                   (duplicate-element-help (rest lopi) curNum (add1 count))
                                                                   (duplicate-element-help (rest lopi) (first lopi) 1))]))]
                                  (if (empty? lopi)
                                      -1
                                      (duplicate-element-help sorted (first lopi) 0))))





;Tests
(check-contract duplicate-element)
(check-contract duplicate-element-prop)



;; Problem 2
(: common-element-prop (-> (List (List Natural)) True))
(define (common-element-prop lolopi) (if (= (common-element lolopi) -1)
                                         #t
                                         (local
                                           [(define (member-all num lolopi) (cond [(empty? lolopi) #t]
                                                                                  [(cons? lolopi) (and (member? num (first lolopi))
                                                                                                       (member-all num (rest lolopi)))]))]
                                           (member-all (common-element lolopi) lolopi))))

(: common-element  (-> (List (List Natural)) Integer))
;Purpose: Given a list of lists of positive integers, if there exists an element common to all of them,
;         returns it, or -1 if there is no such element
(define (common-element lolopi) (local
                                  [(define (member-all num lolopi) (cond [(empty? lolopi) #t]
                                                                              [(cons? lolopi) (and (member? num (first lolopi))
                                                                                                   (member-all num (rest lolopi)))]))
                                   (define (common-element-help lop lolopi)
                                           (cond [(empty? lop) -1]
                                                 [(cons? lop) (if (member-all (first lop) lolopi)
                                                                  (first lop)
                                                                  (common-element-help (rest lop) lolopi))]))]
                                  (if (or (empty? lolopi) (empty? (first lolopi)))
                                      -1
                                      (common-element-help (first lolopi) (rest lolopi)))))

;Tests
(check-contract common-element)
(check-contract common-element-prop)

;; Problem 3
(: pair-with-sum-prop (-> (List Integer) Integer True))
(define (pair-with-sum-prop loi target) (local
                                          [(define out (pair-with-sum loi target))]
                                          (or (empty? out)
                                              (and (= (length out) 2)
                                                   (member? (first out) loi)
                                                   (member? (second out) loi)
                                                   (= (+ (first out) (second out)) target)))))


(: pair-with-sum (-> (List Integer) Integer (List Integer)))
;Purpose: Given a list of integers and a target number, returns either a list with exactly two elements that sum to that target number,
;         or an empty list if there are no two elements that sum to the target
(define (pair-with-sum loi target) (local
                                     [(define (pair-with-sum-help int loi) (cond [(empty? loi) '()]
                                                                                 [(cons? loi) (if (= (+ int (first loi)) target)
                                                                                                  (list int (first loi))
                                                                                                  (pair-with-sum-help int (rest loi)))]))]
                                     (cond [(or (empty? loi) (empty? (rest loi))) '()]
                                           [(cons? loi) (if (empty? (pair-with-sum-help (first loi) (rest loi))) 
                                                            (pair-with-sum (rest loi) target)
                                                            (pair-with-sum-help (first loi) (rest loi)))])))




;Tests
(check-contract pair-with-sum)
(check-contract pair-with-sum-prop)










