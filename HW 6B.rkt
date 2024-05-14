#lang lsl

;; Problem 1

;; part p1
(define-struct leaf [value])
(define-struct node [left right])
(define-contract (Tree X) (OneOf (Leaf X) (Node (Tree X) (Tree X))))


(: tree-map (All (X Y) (-> (-> X Y) (Tree X) (Tree Y))))
(define (tree-map f t)
  (cond [(leaf? t) (make-leaf (f (leaf-value t)))]
        [(node? t) (make-node (tree-map f (node-left t))
                              (tree-map f (node-right t)))]))


(check-contract tree-map)
;; part p1


;; Problem 2

;; part p2

(: p2a (All (X) (-> Boolean X X X)))
(define (p2a b x1 x2) (if b
                          x1
                          x2))

(: p2b (All (X) (-> Boolean X X X)))
(define (p2b b x1 x2) (if b
                          x2
                          x1))

(: p2c (All (X) (-> Boolean X X X)))
(define (p2c b x1 x2) x1)

(: p2d (All (X) (-> Boolean X X X)))
(define (p2d b x1 x2) x2)

;; part p2


;; Problem 3

;; part p3

(define-struct rectangle [width height])
(define-contract Rectangle~ (Rectangle Real Real))

(: rectangle-area (-> Rectangle~ Real))
(define (rectangle-area r)
  (* (rectangle-width r) (rectangle-height r)))

(define-struct circle [radius])
(define-contract Circle~ (Circle Real))

; Good enough for NASA
; https://www.wired.com/story/how-much-pi-do-you-really-need/
(define PI 3.141592653589793)

(: circle-area (-> Circle~ Real))
(define (circle-area c)
  (* (sqr (circle-radius c)) PI))

(define-contract ShapeArea (Exists (S) (Tuple S (-> S Real))))

(: rectangle-with-area (-> Rectangle~ ShapeArea))
(define (rectangle-with-area r) (list r rectangle-area))

(check-contract rectangle-with-area)

(: circle-with-area (-> Circle~ ShapeArea))
(define (circle-with-area c) (list c circle-area))

(check-contract circle-with-area)
;; part p3