#lang lsl
;; Problem 1
(: cyclic-shuffle-prop (-> String True))
;Pupose: Formal specification of cyclic-shuffle-prop
(define (cyclic-shuffle-prop s) (local
                                  [(define master (string-append s s))
                                   (define output (cyclic-shuffle s))]
                                  (and (= (string-length output)
                                          (string-length s))
                                       (string-contains? master output))))


;(random x y) generates a random number between x and y-1
(: cyclic-shuffle (-> String String))
;Purpose: Takes a string and shifts its contents a random number of places
;         i.e., moving a character from the end to the beginning
(define (cyclic-shuffle s) (local
                             [(define randStartIndex (if (= (string-length s) 0)
                                                         0
                                                         (random 0 (string-length s))))]
                             (string-append
                              (substring s randStartIndex (string-length s))
                              (substring s 0 randStartIndex))))


;Test 
(check-contract cyclic-shuffle-prop)



;; Problem 2

(: gcd (-> Natural Natural Natural))
;Purpose: Finds the greatest common divisor of 2 given numbers
;         (returns the largest natural number that divides both inputs)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(: gcd-prop (-> Natural Natural True))
; Purpose: Formal spec for gcd function
(define (gcd-prop x y) (local
                         [(define out (gcd x y))
                          (define lon (range (add1 out) (add1 (min x y))))
                          (define (noOtherGCD lon x y) (cond [(empty? lon) #t]
                                                             [(cons? lon) (if (and (= (modulo x (first lon)) 0)
                                                                                   (= (modulo y (first lon)) 0))
                                                                              #f
                                                                              (noOtherGCD (rest lon) x y))]))]
                         (and (= (modulo x out) 0)
                              (= (modulo y out) 0)
                              (noOtherGCD lon x y))))
;Test - 
(check-contract gcd-prop)




;; Problem 3

(: find-majority-prop (-> (List Natural) True))
;Prupose: Formal spec for the find-majority function
(define (find-majority-prop lon) (local
                                   [(define majority-out (find-majority lon))
                                    (define (counter num lon) (cond [(empty? lon) 0]
                                                                    [(cons? lon) (if (= (first lon) num)
                                                                                     (+ 1 (counter num (rest lon)))
                                                                                     (counter num (rest lon)))]))]
                                   (if (or (= majority-out -1)
                                           (> (counter majority-out lon) (/ (length lon) 2)))
                                       #t
                                       #f)))






(: find-majority (-> (List Natural) Integer))
;Purpose: Takes a list and returns a majority element (one that appears more than half the time in the list)
;         If no such element exists, returns -1

;Note: Sorry to whoever's grading this: I wrote this function before I read that we are
;      suggested to use the Boyer-Moore majority voting algorithm, so its a mess, but it works
;      and the logic makes sense, its just not clean or readable. Didn't want to change it afterwards because
;      I spent quite a bit of time on it :)
(define (find-majority lon) (local
                              [(define sorted-list (sort lon <))
                               (define (find-majority/acc lon max-count max-num cur-count cur-num len)
                                 (cond [(empty? lon) (local
                                                       [(define new-max-count (if (> cur-count max-count)
                                                                                  cur-count
                                                                                  max-count))
                                                        (define new-max-num (if (> cur-count max-count)
                                                                                cur-num
                                                                                max-num))]
                                                       
                                                           
                                                       (if (> new-max-count (/ len 2)) new-max-num -1))]
                                       [(= (first lon) cur-num) (find-majority/acc
                                                                 (rest lon)
                                                                 max-count
                                                                 max-num
                                                                 (add1 cur-count)
                                                                 cur-num
                                                                 len)]
                                       [else (local
                                               [(define new-max-count (if (> cur-count max-count)
                                                                          cur-count
                                                                          max-count))
                                                (define new-max-num (if (> cur-count max-count)
                                                                        cur-num
                                                                        max-num))
                                                (define new-cur-num (first lon))]
                                               (find-majority/acc
                                                (rest lon)
                                                new-max-count
                                                new-max-num
                                                1
                                                new-cur-num
                                                len))]))]
                              (if (empty? sorted-list) -1 (find-majority/acc sorted-list 0 -1 0 (first sorted-list) (length sorted-list)))))

(check-contract find-majority-prop 1000)






