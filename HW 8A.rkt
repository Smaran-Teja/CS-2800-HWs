#lang lsl

;; Problem 1

;; part p1
(define-struct tv [T F AND OR NOT])

(define-contract (TruthVal A) (Tv A
                                  A
                                  (-> A A A)
                                  (-> A A A)
                                  (-> A A)))
;; part p1


;; part p1a
(: BOOL-TV (TruthVal Boolean))
(define BOOL-TV (make-tv #t #f (lambda (x y) (and x y)) (lambda (x y) (or x y)) (lambda (x) (not x))))
;; part p1a


;; Problem 2:


;; part p2
(define-contract Variable Natural)

(define-struct n (var))
;; a negated variable

(define-contract VariableClause (OneOf Variable (N Variable)))
;; a Variable is either a natural number or a negated one

(define-contract (Formula V) (List V))
(define-contract (CNF V) (List (Formula V)))

;; part p2


;; part p2b
(: get-num (-> VariableClause Natural))
;Returns the natural number associated with a variable or its negation
(define (get-num v) (if (n? v)
                        (n-var v)
                        v))

(: maximum (-> (List VariableClause) Natural))
(define (maximum lon) (cond [(empty? lon) 0]
                            [(cons? lon) (if (> (get-num (first lon)) (maximum (rest lon)))
                                             (get-num (first lon))
                                             (maximum (rest lon)))]))



(: variable-upper-bound (-> (CNF VariableClause) Variable))
(define (variable-upper-bound cnf) (local
                                     [(define max-list (map (lambda (x) (maximum x)) cnf))]
                                     (maximum max-list)))





(check-expect (variable-upper-bound (list (list (make-n 1) 3 15 0 (make-n 5))
                                          (list (make-n 2) 7 3 0 (make-n 20))
                                          (list (make-n 4) 9 8 0 (make-n 30))
                                          (list (make-n 9) 1 7 0 (make-n 41)))) 41)
(check-expect (variable-upper-bound (list)) 0)
(check-expect (maximum (list)) 0)
(check-expect (maximum (list (make-n 1) 3 15 0 (make-n 5))) 15)


;; part p2b

;; Problem 3:

;; part p3
(: eval (All (A) (-> (TruthVal A)
                     (CNF A)
                     A)))

(define (eval api cnf) (local [(define evaluated-formulas (map (lambda (x) (eval-formula api x)) cnf))]
                         (cond [(empty? evaluated-formulas) (tv-T api)]
                               [(cons? evaluated-formulas) ((tv-AND api) (first evaluated-formulas)
                                                                        (eval api (rest cnf)))])))


(: eval-formula (All (A) (-> (TruthVal A)
                             (Formula A)
                             A)))
(define (eval-formula api formula) (cond [(empty? formula) (tv-F api)]
                                         [(cons? formula) ((tv-OR api) (first formula)
                                                                 (eval-formula api (rest formula)))]))


(test-suite
 "eval"

 (check-expect (eval-formula BOOL-TV (list #f #f #f #t)) #t)
 (check-expect (eval-formula BOOL-TV (list #f #f #f #f)) #f)
 (check-expect (eval-formula BOOL-TV (list)) #f)

 (check-expect (eval BOOL-TV (list (list #f #f #f #t)
                                   (list #f #t #f #t)
                                   (list #f #t #f #t)
                                   (list #f #f #f #f))) #f)

 (check-expect  (eval BOOL-TV (list (list #f #f #f #t)
                                    (list #f #t #f #t)
                                    (list #f #t #f #t)
                                    (list #f #f #t #f))) #t)

 (check-expect (eval BOOL-TV (list)) #t)
 )

;; part p3


;; Problem 4:

;; part p4
(: subst (All (A) (-> (TruthVal A)
                      (List (Tuple Variable A))
                      (CNF VariableClause)
                      (CNF A))))
(define (subst api vars cnf) (local
                               [(define translated-formulas (map (lambda (x) (subst-formula api vars x)) cnf))]
                               (foldr cons '() translated-formulas)))



(: subst-formula (All (A) (-> (TruthVal A)
                      (List (Tuple Variable A))
                      (Formula VariableClause)
                      (Formula  A))))
(define (subst-formula api vars formula) (local
                                           [(define translated-variables (map (lambda (x) (get-var api x vars)) formula))]
                                           (foldr cons '() translated-variables)))





(: get-var (All (A) (-> (TruthVal A) VariableClause (List (Tuple Variable A)) A)))
(define (get-var api num map) (cond [(empty? map) "Error, variable doesn't exist"]
                                    [(= (first (first map)) (get-num num)) (if (n? num)
                                                                               ((tv-NOT api) (second (first map)))
                                                                               (second (first map)))]
                                    [else (get-var api num (rest map))]))







(test-suite
 "subst"

 (check-expect (get-var BOOL-TV  15 (list (list 10 #f) (list 20 #t) (list 15 #f))) #f)
 (check-expect (get-var BOOL-TV  10 (list (list 10 #t) (list 20 #t) (list 15 #f))) #t)
 (check-expect (get-var BOOL-TV  (make-n 20) (list (list 10 #f) (list 20 #f) (list 15 #f))) #t)

 (check-expect (subst-formula BOOL-TV
                              (list (list 10 #f) (list 20 #f) (list 15 #f))
                              (list (make-n 20) (make-n 10) 10 20 15 (make-n 15))) (list #t #t #f #f #f #t))

 (check-expect (subst-formula BOOL-TV
                              (list (list 10 #t) (list 20 #t) (list 15 #t))
                              (list (make-n 20) (make-n 10) 10 20 15 (make-n 15))) (list #f #f #t #t #t #f))

 (check-expect (subst-formula BOOL-TV
                              (list (list 10 #t) (list 20 #t) (list 15 #t))
                              '()) '())

 (check-expect (subst BOOL-TV
                      (list (list 10 #t) (list 20 #t) (list 15 #t))
                      (list (list (make-n 20) (make-n 10) 10 20 15 (make-n 15))
                            (list 20 10 10 20 15 15)
                            (list (make-n 15) (make-n 10) 10 20 15 (make-n 15))
                            (list (make-n 20) 10 (make-n 20) 15 20 (make-n 10))))
               (list (list #f #f #t #t #t #f)
                     (list #t #t #t #t #t #t)
                     (list #f #f #t #t #t #f)
                     (list #f #t #f #t #t #f)))

 (check-expect (subst BOOL-TV
                      (list (list 10 #t) (list 20 #t) (list 15 #t))
                      (list))
               '())
 
 (check-expect (subst BOOL-TV
                      (list (list 1 #t) (list 2 #f) (list 3 #t))
                      (list (list (make-n 2) (make-n 1) 1 2 3 (make-n 1))
                            (list 2 1 1 2 1 3)
                            (list (make-n 1) (make-n 1) 1 2 1 (make-n 1))
                            (list (make-n 2) 1 (make-n 2) 3 2 (make-n 1))
                            '()))
               (list (list #t #f #t #f #t #f)
                     (list #f #t #t #f #t #t)
                     (list #f #f #t #f #t #f)
                     (list #t #t #t #t #f #f)
                     '()))
 
)

;; part p4

;; Problem 5:

;; part p5
(: all-tvs (All (X) (-> X
                        X
                        Variable
                        (List (List (Tuple Variable X))))))
(define (all-tvs T F n) 
  (local ((define (all-tvs-local n) 
            (cond 
              [(zero? n) '(())] 
              [else 
               (let ([xs (all-tvs-local (sub1 n))])  
                 (append (map (λ (x) (cons (list (sub1 n) T) x)) xs) 
                         (map (λ (x) (cons (list (sub1 n) F) x)) xs)))]))) 
    (map reverse (all-tvs-local n)))) 

(test-suite
 "all-tvs"
(check-expect (all-tvs "a" "b" 3)
              '(((0 "a") (1 "a") (2 "a"))
                ((0 "b") (1 "a") (2 "a"))
                ((0 "a") (1 "b") (2 "a"))
                ((0 "b") (1 "b") (2 "a"))
                ((0 "a") (1 "a") (2 "b"))
                ((0 "b") (1 "a") (2 "b"))
                ((0 "a") (1 "b") (2 "b"))
                ((0 "b") (1 "b") (2 "b"))))
              
)

(: sat (All (A) (-> (TruthVal A) 
                    (CNF VariableClause) 
                    A))) 

(define (sat api cnf) (local
                        [(define all-arrangements (all-tvs (tv-T api)
                                                           (tv-F api)
                                                           (add1 (variable-upper-bound cnf))))
                         
                         (define vals (map (lambda (x) (eval api (subst api x cnf))) all-arrangements))
                         
                         (define (my-ormap vals) (cond [(empty? vals) (tv-F api)]
                                                       [(cons? vals) ((tv-OR api)
                                                                        (first vals)
                                                                        (my-ormap (rest vals)))]))]
                        (my-ormap vals)))







(test-suite
 "sat"
 (check-expect (sat BOOL-TV (list (list 1 2 3 4 5)
                                  (list (make-n 1))
                                  (list (make-n 2))
                                  (list (make-n 3))
                                  (list (make-n 4))
                                  (list (make-n 5)))) #f)
 
 (check-expect (sat BOOL-TV (list (list 0 2 3 4 5)
                                  (list (make-n 1))
                                  (list (make-n 2))
                                  (list (make-n 3))
                                  (list (make-n 4))
                                  (list (make-n 5)))) #t)
 
 (check-expect (sat BOOL-TV (list (list 1 2)
                                  (list (make-n 1))
                                  (list (make-n 2)))) #f)
 
 (check-expect (sat BOOL-TV (list (list 1 2)
                                  (list (make-n 1)))) #t)

 (check-expect (sat BOOL-TV (list (list 1)
                                  (list (make-n 1) (make-n 2)))) #t)

 (check-expect (sat BOOL-TV '()) #t)

 (check-expect (sat BOOL-TV (list '()
                                  (list (make-n 1) (make-n 2)))) #f)

)


;; part p5


;; Problem 6:

;; part p6

(: ONEZERO-TV (TruthVal (OneOf (Constant 0) (Constant 1))))
(define ONEZERO-TV (make-tv 1
                            0
                            (lambda (x y) (if (and (= x 1) (= y 1))
                                              1
                                              0))

                            (lambda (x y) (if (or (= x 1) (= y 1))
                                              1
                                              0))
                            
                            (lambda (x) (if (= x 1)
                                            0
                                            1))))

(: STRING-TV (TruthVal String))
(define STRING-TV (make-tv "true"
                           "false"
                           (lambda (x y) (if (and (string=? x "true") (string=? y "true"))
                                             "true"
                                             "false"))

                           (lambda (x y) (if (or (string=? x "true") (string=? y "true"))
                                             "true"
                                             "false"))
                            
                           (lambda (x) (if (string=? x "true")
                                           "false"
                                           "true"))))

(define-struct true ())
(define-struct false ())

(: STRUCT-TV (TruthVal (OneOf (True) (False))))
(define STRUCT-TV (make-tv (make-true)
                           (make-false)
                           (lambda (x y) (if (and (true? x) (true? y))
                                             (make-true)
                                             (make-false)))

                           (lambda (x y) (if (or (true? x) (true? y))
                                             (make-true)
                                             (make-false)))
                            
                           (lambda (x) (if (true? x)
                                           (make-false)
                                           (make-true)))))

(define t (lambda (x y) x))
(define f (lambda (x y) y))

(: LAMBDA-TV (TruthVal (All (X) (-> X X X))))
(define LAMBDA-TV (make-tv t
                           f
                           (lambda (x y) ((x t f) (y t f) f))
                           (lambda (x y) ((x t f) t (y t f)))
                           (lambda (x) (x f t))))




 
(test-suite
 "sat-other"
 (check-expect (sat ONEZERO-TV (list (list 1 2)
                                  (list (make-n 1))
                                  (list (make-n 2)))) 0)
 
 (check-expect (sat ONEZERO-TV (list (list 1 2)
                                  (list (make-n 1)))) 1)

 (check-expect (sat ONEZERO-TV (list (list 1)
                                  (list (make-n 1) (make-n 2)))) 1)

 (check-expect (sat ONEZERO-TV '()) 1)

 (check-expect (sat ONEZERO-TV (list '()
                                  (list (make-n 1) (make-n 2)))) 0)




 
 
 (check-expect (sat STRING-TV (list (list 1 2)
                                    (list (make-n 1))
                                    (list (make-n 2)))) "false")
 
 (check-expect (sat STRING-TV (list (list 1 2)
                                    (list (make-n 1)))) "true")

 (check-expect (sat STRING-TV (list (list 1)
                                    (list (make-n 1) (make-n 2)))) "true")

 (check-expect (sat STRING-TV '()) "true")

 (check-expect (sat STRING-TV (list '()
                                  (list (make-n 1) (make-n 2)))) "false")



 

 (check-expect (sat STRUCT-TV (list (list 1 2)
                                    (list (make-n 1))
                                    (list (make-n 2)))) (make-false))
 
 (check-expect (sat STRUCT-TV (list (list 1 2)
                                    (list (make-n 1)))) (make-true))

 (check-expect (sat STRUCT-TV (list (list 1)
                                    (list (make-n 1) (make-n 2)))) (make-true))

 (check-expect (sat STRUCT-TV '()) (make-true))

 (check-expect (sat STRUCT-TV (list '()
                                  (list (make-n 1) (make-n 2)))) (make-false))





 (check-expect ((sat LAMBDA-TV (list (list 1 2)
                                    (list (make-n 1))
                                    (list (make-n 2)))) #t #f) #f)
 
 (check-expect ((sat LAMBDA-TV (list (list 1 2)
                                    (list (make-n 1)))) #t #f) #t)

 (check-expect ((sat LAMBDA-TV (list (list 1)
                                    (list (make-n 1) (make-n 2)))) #t #f) #t)

 (check-expect ((sat LAMBDA-TV '()) #t #f) #t)

 (check-expect ((sat LAMBDA-TV (list '()
                                  (list (make-n 1) (make-n 2)))) #t #f) #f)
)
;; part p6

