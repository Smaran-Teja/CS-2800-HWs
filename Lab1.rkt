#lang lsl
(define-struct leaf [])
(define-struct node [k v l r])
(define-contract (Tree K V) (OneOf (Leaf) (Node K V (Tree K V) (Tree K V))))



(define-struct sop [empty singleton union intersect member?])
(define-contract (SOP~ Set Elt) (Sop Set
                                     (-> Elt Set)
                                     (-> Set Set Set)
                                     (-> Set Set Set)
                                     (-> Elt Set Boolean)))


;Returns a list of all elements common to both lists l1 and l2
(define (intersect l1 l2) (cond [(empty? l1) '()]
                                [(list? l1) (if (member? (first l1) l2)
                                                (cons (first l1) (intersect (rest l1) l2))
                                                (intersect (rest l1) l2))]))

;Finds the set union without duplicates of 2 sets.
(define (union l1 l2) (cond [(empty? l1) l2]
                            [(list? l1) (if (member? (first l1) l2)
                                            (union (rest l1) l2)
                                            (union (rest l1) (cons (first l1) l2)))]))
(: SOP-LIST (SOP~ (List String) String))
(define SOP-LIST (make-sop '()
                           (lambda (x) (list x))
                           (lambda (x y) (union x y))
                           (lambda (x y) (intersect x y))
                           (lambda (x y) (member? x y))))



(: tree-keys (All (Set V)
                  (-> (SOP~ Set String)
                      (Tree String V)
                      Set)))


(define (tree-keys sop t) (cond [(leaf? t) (sop-empty sop)]
                                [(node? t) ((sop-union sop) ((sop-singleton sop) (node-k t))
                                                            ((sop-union sop) (tree-keys sop (node-l t))
                                                                             (tree-keys sop (node-r t))))]))
 


(define T1 (make-node "a" 1 (make-node "b" 2 (make-leaf) (make-leaf)) (make-leaf)))
(define T1-KEYS (list "a" "b"))
 
(define (t1-prop sop) (local [(define set (tree-keys sop T1))]
                             (and ((sop-member? sop) "a" set)
                                  ((sop-member? sop) "b" set))))

(check-satisfied SOP-LIST t1-prop)


