#lang lsl

(define-struct affine-error (v))
(define-struct affine-container (thk))
(define (affine v)
  (let ([a #f])
    (make-affine-container
      (lambda ()
        (if a
            (raise (make-affine-error v))
            (begin (set! a #t)
                   v))))))


(define (affine-get a)
  ((affine-container-thk a))) 

(define-contract (Single T)
  (Immediate (check affine-container?)
             (generate (lambda (fuel)
                         (affine (contract-generate T fuel))))))

;; part p0a
(define-struct item (name))
(define-contract Item~ (Item String))
;; part p0a

;; part p0b

(define-contract Inventory (List (Single Item~)))
;; part p0b

;; Problem 1

;; part p1
(: inventory-map (-> (-> Item~ Item~) Inventory Inventory))
(define (inventory-map f i) (cond [(empty? i) empty]
                                  [(cons? i) (cons (affine (f (affine-get (first i))))
                                                   (inventory-map f (rest i)))]))
;; part p1




;(define mapped (inventory-map (lambda (x) (make-item (string-append (item-name x) "a"))) I-1))

;(affine-get (first mapped))

;; Problem 2

;; part p2
(define (unpack l) (cond [(empty? l) empty]
                         [(cons? l) (cons (affine-get (first l)) (unpack (rest l)))]))

(define (pack l) (cond [(empty? l) empty]
                       [(cons? l) (cons (affine (first l)) (pack (rest l)))]))

(define (get-passed p? l) (cond [(empty? l) empty]
                                [(cons? l) (if (p? (first l))
                                               (cons (first l)
                                                     (get-passed p? (rest l)))
                                               (get-passed p? (rest l)))]))

(: inventory-partition (-> (-> Item~ Boolean) Inventory (Tuple Inventory Inventory)))
(define (inventory-partition p? i) (local
                                     [(define unpacked (unpack i))
                                      (define passed (get-passed p? unpacked))
                                      (define failed (get-passed (lambda (x) (not (p? x))) unpacked))]
                                     (list (pack passed) (pack failed))))



;(inventory-partition (lambda (x) (= (string-length (item-name x)) 1)) I-1)
;; part p2


;; Problem 3

;; part p3
(: inventory-get (-> (-> Item~ Boolean) Inventory (Tuple (Maybe (Single Item~)) Inventory)))
(define (inventory-get p? i) (local
                               [(define partition (inventory-partition p? i))
                                (define passed (if (>= (length (first partition)) 1)
                                                        (first (first partition))
                                                        #f))]
                               
                               (list passed (append (if (boolean? passed)
                                                        (first partition)
                                                        (rest (first partition)))
                                                    (second partition)))))

;(inventory-get (lambda (x) (= (string-length (item-name x)) 1)) I-1)
;; part p3


;; Problem 4


;; part p4
(: craft-hammer (-> Inventory Inventory))
(define (craft-hammer i) (local
                           [(define get-w1 (inventory-get (lambda (x) (string=? (item-name x) "wood")) i))
                            (define w1 (first get-w1))
                            
                            (define get-w2 (inventory-get (lambda (x) (string=? (item-name x) "wood")) (second get-w1)))
                            (define w2 (first get-w2))
                            
                            (define get-s (inventory-get (lambda (x) (string=? (item-name x) "stone")) (second get-w2)))
                            (define s1 (first get-s))]
                           (if (or (boolean? w1) (boolean? w2) (boolean? s1))
                               (second get-s)
                               (cons (affine (make-item "hammer"))
                                     (second get-s)))))















(define I-1 (list (affine (make-item "wood"))
                  (affine (make-item "blah"))
                  (affine (make-item "stone"))
                  (affine (make-item "blah"))))

(craft-hammer I-1)
;; part p4
