#lang lsl

;; Problem 1
(: dist (-> Real Real Real))
(define (dist x y) (sqrt (+ (* x x) (* y y))))

(check-contract dist)


;; Problem 2
(: cube-vol (-> Real Real))
(define (cube-vol side-len) (* side-len side-len side-len))

(check-contract cube-vol)

;; Problem 3
(: nor (-> Boolean Boolean Boolean))

(define (nor p q) (not (or p q)))

(check-contract nor)


;; Problem 4

(: string-hide (-> String Natural String))
(define (string-hide str pos) (if (> pos (string-length str))
                                  str
                                  (string-append (substring str 0 (- pos 1)) "_" (substring str pos (string-length str)))))
(check-contract string-hide)