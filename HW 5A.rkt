#lang lsl

;; Problem 1:

;; part p1a
(define-contract Bit (OneOf (Constant 0) (Constant 1)))

(define-contract Key (Tuple Bit Bit Bit Bit Bit Bit))

(define-contract Message (Tuple Bit Bit Bit Bit Bit Bit))
;; part p1a

;; part p1b
(: xor (-> Bit Bit Bit))
(define (xor b1 b2)
  (modulo (+ b1 b2) 2))
(check-expect (xor 0 0) 0)
(check-expect (xor 0 1) 1)
(check-expect (xor 1 0) 1)
(check-expect (xor 1 1) 0)

(: xor-list (-> [List Bit] [List Bit] [List Bit]))
(define (xor-list l1 l2)
  (map xor l1 l2))
(check-expect (xor-list (list 1 0 0) (list 1 1 1)) (list 0 1 1))
(check-expect (xor-list (list 0 0 0) (list 0 0 0)) (list 0 0 0))

(: encrypt (-> Message Key Message))
(define encrypt xor-list)

(: decrypt (-> Message Key Message))
(define decrypt xor-list)
;; part p1b

;; part p1c
(: xor-perfect-prop (-> Message Message True))
(define (xor-perfect-prop encr-msg arbitrary-msg) (local
                                                    [(define (findKeyBit ob eb) (if (= (+ ob eb) 1)
                                                                                    1
                                                                                    0))
                                                     (define (findKey om em)
                                                       (cond
                                                         [(empty? em) '()]
                                                         [(cons? em) (cons (findKeyBit (first om)
                                                                                             (first em))
                                                                                 (findKey (rest om)
                                                                                          (rest em)))]))
                                                     (define key (findKey encr-msg arbitrary-msg))
                                                     (define decryptedKey (decrypt encr-msg key))]
                                                    (equal? decryptedKey arbitrary-msg)))

;; 0 1  -> 1
;; 1 0  -> 1
;; 1 1  -> 0
;; 0 0  -> 0

(check-contract xor-perfect-prop)
;; part p1c


;; Problem 2


;; part p2a
(define CORRECT-PASSWORD
  (explode "a7he29hdee"))

(define-contract Password (lambda (s) (and (list? s) (andmap string? s))))

(: password=? (-> Password Password Boolean))
(define (password=? l1 l2)
  (cond [(and (empty? l1) (empty? l2)) #t]

        
        [(and (empty? l1) (cons? l2))
         (and (password=? '() (rest l2))
              (string=? "a" "b"))]
        
        [(and (cons? l1) (empty? l2))
         (and (password=? (rest l1) '())
              (string=? "a" "b"))]
        
        [(and (cons? l1) (cons? l2))
         (and (password=? (rest l1) (rest l2))
              (string=? (first l1) (first l2)))]))

(: check-password (-> Password Boolean))
(define (check-password p)
  (password=? CORRECT-PASSWORD p))
;; part p2a

;; part p2c


(: timing-spec (-> String String True))
(define (timing-spec s1 s2)
  (local  [(define p1 (explode s1))
           (define p2 (explode s2))
           (define thunkP1 (lambda () (check-password p1)))
           (define thunkP2 (lambda () (check-password p2)))]
    (= (ticks thunkP1) (ticks thunkP2))))

    

(check-contract timing-spec)
;; part p2c
