#lang lsl


;; part p1a
(define EMPTY 'empty)
(define WALL 'wall)
(define PLAYER 'player)
(define EXIT 'exit)

(define-contract Cell (OneOf (Constant EMPTY)
                             (Constant WALL)
                             (Constant PLAYER)
                             (Constant EXIT)))
;; part p1a


;; part p1b
(define-struct posn (x y))
(define-struct at (c pos))

(define-contract Posn~ (Posn Natural Natural))
(define-contract At~ (At Cell Posn~))

(define-contract Maze (List (At Cell Posn~)))
;; part p1b

;; part p1c
(define-contract SExp (OneOf Symbol
                             Integer
                             Boolean
                             String
                             (List SExp)))
;; part p1c

;; part p3
(define-struct invalid-sexp (sexp))
;; part p3

;; Problem 1

(define options (list WALL EMPTY PLAYER EXIT))
(define (random-cell) (list-ref options (random (length options))))

;Tests
(check-expect (member? (random-cell) options) #t)
(check-expect (member? (random-cell) options) #t)
(check-expect (member? (random-cell) options) #t)


(define (random-at n) (make-at (random-cell) (make-posn (random n) (random n))))

;Tests
(check-expect (< (posn-x (at-pos (random-at 5))) 5) #t)
(check-expect (< (posn-y (at-pos (random-at 5))) 5) #t)
(check-expect (< (posn-x (at-pos (random-at 100))) 100) #t)
(check-expect (< (posn-y (at-pos (random-at 100))) 100) #t)




(: random-maze (-> Natural Maze))
(define (random-maze n) (local [(define (random-maze-help len) (cond [(zero? len) '()]
                                                                     [else (cons (random-at n)
                                                                                 (random-maze-help (sub1 len)))]))]
                          (random-maze-help (* n n))))
;Tests
(check-expect (length (random-maze 2)) 4)
(check-expect (length (random-maze 5)) 25)
(check-expect (length (random-maze 10)) 100)
(check-expect (length (random-maze 50)) 2500)

;; Problem 2
(define A1 (make-at 'wall (make-posn 0 0)))
(define A2 (make-at 'player (make-posn 0 1)))
(define A3 (make-at 'exit (make-posn 1 0)))
(define A4 (make-at 'empty (make-posn 1 1)))

(define M1 (list A4 A3 A2 A1))
(define M2 (list A1 A2 A2 A3))
(define M3 (list A1 A2 A3 A3))
(define M4 (list A1 A1 A4 A4))

(: single-pred? (-> Maze Boolean (-> At~ Boolean) Boolean)) 
(define (single-pred? maze exists pred) (cond
                                          [(empty? maze) exists]
                                          [(pred (first maze)) (if exists
                                                                   #f
                                                                   (single-pred? (rest maze) #t pred))]
                                          [else (single-pred? (rest maze) exists pred)]))


(define (single-player-pred at) (equal? (at-c at) 'player))
(define (single-exit-pred at) (equal? (at-c at) 'exit))

;Tests
(check-expect (single-pred? M1 #f single-player-pred) #t)
(check-expect (single-pred? M2 #f single-player-pred) #f)
(check-expect (single-pred? M3 #f single-player-pred) #t)
(check-expect (single-pred? M4 #f single-player-pred) #f)

(check-expect (single-pred? M1 #f single-exit-pred) #t)
(check-expect (single-pred? M2 #f single-exit-pred) #t)
(check-expect (single-pred? M3 #f single-exit-pred) #f)
(check-expect (single-pred? M4 #f single-exit-pred) #f)


(: single-player-exit? (-> Maze Boolean))
(define (single-player-exit? maze) (and (single-pred? maze #f single-player-pred)
                                        (single-pred? maze #f single-exit-pred)))


;Tests
(check-expect (single-player-exit? M1) #t)
(check-expect (single-player-exit? M2) #f)
(check-expect (single-player-exit? M3) #f)
(check-expect (single-player-exit? M4) #f)


(: random-maze-2-time-taking (-> Natural Maze))
(define (random-maze-2-time-taking n) (local [(define test (random-maze n))]
                                        (if (single-player-exit? test)
                                            test
                                            (random-maze-2 n))))


(: random-maze-2 (-> Natural Maze))
(define (random-maze-2 n) (local
                            [(define (random-maze-help len exists-exit exists-player)
                               (cond [(zero? len) '()]
                                     [else (local
                                             [(define randAt (random-at n))]
                                             (cond
                                               [(and exists-exit (single-exit-pred randAt))
                                                (random-maze-help len exists-exit exists-player)]
                                               
                                               [(and exists-player (single-player-pred randAt))
                                                (random-maze-help len exists-exit exists-player)]
                                               
                                               [(single-player-pred randAt)
                                                (cons randAt (random-maze-help (sub1 len) exists-exit #t))]
                                               
                                               [(single-exit-pred (random-at n))
                                                (cons randAt (random-maze-help (sub1 len) #t exists-player))]

                                               [else
                                                (cons randAt (random-maze-help (sub1 len) exists-exit exists-player))]))]))
                             (define maze (random-maze-help (* n n) #f #f))]
                            
                            (if (single-player-exit? maze)
                                maze
                                (random-maze-2 n))))

(check-expect (length (random-maze-2 2)) 4)
(check-expect (length (random-maze-2 4)) 16)
(check-expect (length (random-maze-2 10)) 100)
(check-expect (length (random-maze-2 5)) 25)



;; Problem 3
(: sexp->cell (Function (arguments [_ SExp]) (result Cell) (raises invalid-sexp)))
(define (sexp->cell sexp) (cond [(equal? sexp 'X) WALL]
                                [(equal? sexp '_) EMPTY]
                                [(equal? sexp 'P) PLAYER]
                                [(equal? sexp 'E) EXIT]
                                [else (raise (make-invalid-sexp sexp))]))

;Tests -
(check-expect (sexp->cell 'X) WALL)
(check-expect (sexp->cell '_) EMPTY)
(check-expect (sexp->cell 'P) PLAYER)
(check-expect (sexp->cell 'E) EXIT)
(check-raises (sexp->cell 'F))

(: cell->sexp (-> Cell SExp))
(define (cell->sexp cell) (cond [(equal? cell WALL) 'X]
                                [(equal? cell EMPTY) '_]
                                [(equal? cell PLAYER) 'P]
                                [(equal? cell EXIT) 'E]))

;Tests -
(check-expect (cell->sexp WALL) 'X)
(check-expect (cell->sexp EMPTY) '_)
(check-expect (cell->sexp PLAYER) 'P)
(check-expect (cell->sexp EXIT) 'E)




;; Problem 4
(: cell-roundtrip-prop (-> Cell True))
(define (roundtrip-prop c) (equal? c (sexp->cell (cell->sexp c))))

(check-contract roundtrip-prop)

;Other direction does not work (given sexp, check sexp->cell->sexp is same as original sexp) because
;sexp may be invalid and so may raise an error when converting sexp->cell


;; Problem 5
(: sexp->maze (-> SExp Maze))
(define (sexp->maze sexp) (local
                            [(define (sexp-row->maze row-exp r c) (cond [(empty? row-exp) '()]
                                                                        [else (cons (make-at (sexp->cell (first row-exp))
                                                                                             (make-posn c r))
                                                                                    (sexp-row->maze (rest row-exp) r (add1 c)))]))

                             (define (sexp->maze-help sexp r) (cond [(empty? sexp) '()]
                                                                    [else (append
                                                                           (sexp-row->maze (first sexp) r 0)
                                                                           (sexp->maze-help (rest sexp) (add1 r)))]))]
                            (sexp->maze-help sexp 0)))



(define s1 '((X X P)
             (E X _)
             (_ _ _)))

(define out
  (list (make-at 'wall (make-posn 0 0))
        (make-at 'wall (make-posn 1 0))
        (make-at 'player(make-posn 2 0))
        (make-at 'exit (make-posn 0 1))
        (make-at 'wall (make-posn 1 1))
        (make-at 'empty (make-posn 2 1))
        (make-at 'empty (make-posn 0 2))
        (make-at 'empty (make-posn 1 2))
        (make-at 'empty (make-posn 2 2))))

(check-expect (sexp->maze s1) out)
(check-expect (length (sexp->maze s1)) 9)

;removes items with duplicate posns
(define (remove-dupes m visited) (cond [(empty? m) '()]
                                       [(cons? m) (if (member? (at-pos (first m)) visited)
                                                      (remove-dupes (rest m) visited)
                                                      (cons (first m) (remove-dupes (rest m)
                                                                                    (cons (at-pos (first m)) visited))))]))
(define (maximum l) (cond [(empty? l) -1]
                          [(cons? l) (if (> (first l) (maximum (rest l)))
                                         (first l)
                                         (maximum (rest l)))]))

(define (minimum l) (cond [(empty? l) 999999999999999]
                          [(cons? l) (if (< (first l) (maximum (rest l)))
                                         (first l)
                                         (maximum (rest l)))]))



(define (getElementAt maze psn) (cond [(empty? maze) (make-at WALL psn)]
                                      [(cons? maze) (if (equal? psn (at-pos (first maze)))
                                                        (first maze)
                                                        (getElementAt (rest maze) psn))]))

(: maze->sexp (-> Maze SExp))
(define (maze->sexp m) (local [(define max-x (maximum (map (lambda (x) (posn-x (at-pos x))) m)))
                               (define max-y (maximum (map (lambda (x) (posn-y (at-pos x))) m)))]

                         (build-list (add1 max-y)
                                     (lambda (y) (build-list (add1 max-x)
                                                             (lambda (x) (cell->sexp (at-c (getElementAt m (make-posn x y))))))))))
;tests

(check-expect (maze->sexp out) s1)
(check-expect (length (maze->sexp out)) 3)
(check-expect (length (first (maze->sexp out))) 3)

(define (add-walls maze) (local [(define max-x (maximum (map (lambda (x) (posn-x (at-pos x))) maze)))
                                 (define max-y (maximum (map (lambda (x) (posn-y (at-pos x))) maze)))]

                               (build-list (add1 max-y)
                                           (lambda (y) (build-list (add1 max-x)
                                                                   (lambda (x) (getElementAt maze (make-posn x y))))))))




(define (merge-append l) (cond [(empty? l) '()]
                               [(cons? l) (if (cons? (first l))
                                              (append (first l) (merge-append (rest l)))
                                              (append (list (first l)) (merge-append (rest l))))]))
;; Problem 6

(define (subset a b) (andmap (lambda (x) (member? x b)) a))


(: maze-roundtrip-prop (-> Maze True))
(define (maze-roundtrip-prop maze) (and (subset (sexp->maze (maze->sexp maze))
                                                (merge-append (add-walls maze)))
                                        (subset (merge-append (add-walls maze))
                                                (sexp->maze (maze->sexp maze)))))

;(check-contract maze-roundtrip-prop)


;; Problem 7


;; Problem 8


;; part p8a
(define-contract (Maybe T) (OneOf T (Constant #f)))
;; part p8a