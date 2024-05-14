#lang lsl

;; Problem 1

;; part p1a
(define STARTBAL 1000)
(define MAX-TRANSFERS 20)

(define-struct bank-state-v1 (balance num-transfers other-banks))
(define-contract BS1 (BankStateV1 Natural Natural (List String)))

(define-struct transfer (bal))
(define-contract Transfer~ (Transfer Natural))
;; part p1a

;; part p1b
(: bank-start-v1 (-> (List String) (Action BS1)))
(define (bank-start-v1 others) (local [(define others-exist? (cons? others))
                                       (define rand-other-idx (if others-exist?
                                                                   (random (length others))
                                                                   -1))
                                       (define amt (if others-exist?
                                                       (random STARTBAL)
                                                       0))
                                       (define msg (if others-exist?
                                                       (list (send-packet (list-ref others rand-other-idx)
                                                                          (make-transfer amt)))
                                                       empty))
                                       (define st (make-bank-state-v1 (- STARTBAL amt)
                                                                      0
                                                                      others))]
                                 (action st msg)))
#|
(bank-start-v1 (list "a" "b" "f"))
(bank-start-v1 (list "a" "b" "f"))
(bank-start-v1 (list "a" "b" "f"))
(bank-start-v1 (list "a" "b" "f"))
|#

;; part p1b

;; part p1c
(: bank-receive-v1 (-> BS1 (ReceivePacket Transfer~) (Action BS1)))
(define (bank-receive-v1 st pkt) (local [(define others (bank-state-v1-other-banks st))
                                         (define cur-bal (bank-state-v1-balance st))
                                         (define cur-transfers (bank-state-v1-num-transfers st))

                                         (define new-bal (+ cur-bal (transfer-bal (receive-packet-msg pkt))))
                                         ; balance after receiving money
                                         
                                         (define others-exist? (cons? others))

                                         (define rand-other-idx (if (and others-exist? (< (add1 cur-transfers) MAX-TRANSFERS))
                                                                    (random (length others))
                                                                    -1))
                                         (define amt (if (and others-exist? (< (add1 cur-transfers) MAX-TRANSFERS))
                                                         (random new-bal)
                                                         0))
                                         
                                         (define msg (if (and others-exist? (< (add1 cur-transfers) MAX-TRANSFERS))
                                                         (list (send-packet (list-ref others rand-other-idx)
                                                                            (make-transfer amt)))
                                                         empty))
                                         
                                         (define new-st (make-bank-state-v1
                                                         (- new-bal amt)
                                                         ; balance after receiving money and sending amt
                                                         (add1 cur-transfers)
                                                         others))]
                                   (action new-st msg)))

(define bank-1 (make-bank-state-v1 1000 19 '("a" "b" "c")))
;(bank-receive-v1 bank-1 (receive-packet "a" (make-transfer 500)))
;; part p1c



;; part p1d
(define (bank-process-v1 nm) (process (name nm)
                                      (on-start bank-start-v1)
                                      (on-receive bank-receive-v1)))
;; part p1d

;; part p1e
(define (bank-v1) (local [(define bank-1 (bank-process-v1 "a"))
                          (define bank-2 (bank-process-v1 "b"))
                          (define bank-3 (bank-process-v1 "c"))
                          (define bank-4 (bank-process-v1 "d"))
                          (define bank-5 (bank-process-v1 "e"))]
                    (start-debug first (list bank-1 bank-2 bank-3 bank-4 bank-5))))

;(bank-v1)
;; part p1e


;; Problem 2

;; part p2a
(define UNTIL-SNAPSHOT 10)

(define-struct bank-state (balance num-transfers other-banks snapshot ignored))
(define-contract BS (BankState Natural Natural (List String) (Maybe Natural) (List String)))

(define-struct marker ())

(define-contract Message (OneOf Transfer~ (Marker)))
;; part p2a


;; part p2b
(: bank-start (-> (List String) (Action BS)))
(define (bank-start others) (local [(define others-exist? (cons? others))
                                       (define rand-other-idx (if others-exist?
                                                                   (random (length others))
                                                                   -1))
                                       (define amt (if others-exist?
                                                       (random STARTBAL)
                                                       0))
                                       (define msg (if others-exist?
                                                       (list (send-packet (list-ref others rand-other-idx)
                                                                          (make-transfer amt)))
                                                       empty))
                                       (define st (make-bank-state (- STARTBAL amt)
                                                                      0
                                                                      others
                                                                      #f
                                                                      '()))]
                                 (action st msg)))

;(bank-start (list "a" "b"))

;Given a list of other banks, broadcasts marker messages to the other banks
(define (send-markers l) (cond [(empty? l) empty]
                            [(cons? l) (cons (send-packet (first l)
                                                          (make-marker))
                                             (send-markers (rest l)))]))
;Given a list of other banks and current available balance, makes a trasnfer of random amount to a random bank
(define (send-transfer bal l) (local [(define amt (if (cons? l)
                                                      (random bal)
                                                      0))
                                      (define rand-idx (if (cons? l)
                                                           (random (length l))
                                                           -1))]
                                (if (cons? l)
                                    (list (send-packet (list-ref l rand-idx)
                                                   (make-transfer amt)))
                                    empty)))

(: bank-receive (-> BS (ReceivePacket Message) (Action BS)))
(define (bank-receive st pkt) (local [(define others (bank-state-other-banks st))
                                      (define cur-bal (bank-state-balance st))
                                      (define cur-transfers (bank-state-num-transfers st))
                                      (define cur-snapshot (bank-state-snapshot st))
                                      (define cur-ignored (bank-state-ignored st))

                                      (define is-marker? (marker? (receive-packet-msg pkt)))
                                      (define others-exist? (cons? others))]
                                (cond
                                  ;Case where received marker and dont currently have snapshot in progress
                                  ;Add sender to ignore and copy bal to snapshot, broadcast marker msgs
                                  [(and is-marker? (boolean? cur-snapshot))
                                   (local [(define new-bal cur-bal)
                                           (define new-transfers cur-transfers)
                                           (define new-snapshot cur-bal) ; Copied balance to snapshot
                                           (define new-ignored (cons (receive-packet-from pkt) cur-ignored)) ; Added sender to ignored
                                           (define new-st (make-bank-state new-bal new-transfers others new-snapshot new-ignored))]
                                     (action new-st (send-markers others)))]
                                  
                                  ;Case where received marker, but already currently have snapshot in progress
                                  ;Add sender to ignore and do nothing else
                                  [(and is-marker? (not (boolean? cur-snapshot)))
                                   (local [(define new-bal cur-bal)
                                           (define new-transfers cur-transfers)
                                           (define new-snapshot cur-snapshot)
                                           (define new-ignored (cons (receive-packet-from pkt) cur-ignored)) ; Added sender to ignored
                                           (define new-st (make-bank-state new-bal new-transfers others new-snapshot new-ignored))]
                                     (action new-st '()))]
                                  
                                  ;Case where received transfer amt, and currently have snapshot in progress
                                  ;if not on ignore list, and add transfer amt to both balance and snapshot
                                  ;If on ignore list, do not add amt to snapshot, only add to balance
                                  [(and (not is-marker?) (not (boolean? cur-snapshot)))
                                   (local [(define should-ignore (member? (receive-packet-from pkt) cur-ignored))
                                           (define receive-amt (transfer-bal (receive-packet-msg pkt)))
                                           (define new-bal (+ cur-bal receive-amt)) ; Added amt to balance
                                           (define new-transfers (add1 cur-transfers)) 
                                           (define new-snapshot (if should-ignore
                                                                    cur-snapshot
                                                                    (+ cur-snapshot receive-amt))) ; Added amt to snapshot
                                           
                                           (define transfer-msg (if (< new-transfers MAX-TRANSFERS)
                                                                    (send-transfer new-bal others)
                                                                    empty))
                                           
                                           (define amt-out (if (< new-transfers MAX-TRANSFERS)
                                                               (transfer-bal (send-packet-msg (first transfer-msg)))
                                                               0))
                                           
                                           (define final-bal (- new-bal amt-out))
                                           (define new-ignored cur-ignored)
                                           (define new-st (make-bank-state final-bal new-transfers others new-snapshot new-ignored))]
                                     (action new-st transfer-msg))]

                                  ;Case where received transfer amt, and no snapshot in progress
                                  ;Regular transfer case, may start snapshot with 1/2 probability if crossed  UNTIL-SNAPSHOT, and sends transfer as well
                                  [(and (not is-marker?) (boolean? cur-snapshot))
                                   (local [(define receive-amt (transfer-bal (receive-packet-msg pkt)))
                                           (define new-bal (+ cur-bal receive-amt)) ; after added amt to balance
                                           (define new-transfers (add1 cur-transfers)) ; Increment receieve counter
                                           (define new-snapshot cur-snapshot)
                                           (define new-ignored cur-ignored)
                                           
                                           (define transfer-msg (if (< new-transfers MAX-TRANSFERS)
                                                                    (send-transfer new-bal others)
                                                                    empty))
                                           
                                           (define amt-out (if (< new-transfers MAX-TRANSFERS)
                                                               (transfer-bal (send-packet-msg (first transfer-msg)))
                                                               0))
                                           
                                           (define final-bal (- new-bal amt-out))
                                           (define new-st (make-bank-state final-bal new-transfers others new-snapshot new-ignored))]
                                     (if (>= new-transfers UNTIL-SNAPSHOT)
                                         (local [(define rand-initiate-snap (= (random 2) 1))
                                                 (define marker-msgs (if rand-initiate-snap
                                                                         (send-markers others)
                                                                         empty))
                                                 
                                                 (define final-snapshot (if rand-initiate-snap
                                                                            new-bal
                                                                            new-snapshot))
                                                
                                                 (define final-st (make-bank-state final-bal new-transfers others final-snapshot new-ignored))
                                                 
                                                 (define final-msg (append marker-msgs transfer-msg))]
                                           (action final-st final-msg))
                                         (action new-st transfer-msg)))])))

;(define-struct bank-state (balance num-transfers other-banks snapshot ignored))
(define bank-A (make-bank-state 7 15 (list "b" "c") 13 '("b")))
(bank-receive bank-A (receive-packet "c" (make-transfer 5)))


(define (bank-process nm) (process (name nm)
                                   (on-start bank-start)
                                   (on-receive bank-receive)))

(define (bank) (local [(define bank-1 (bank-process "a"))
                       (define bank-2 (bank-process "b"))
                       (define bank-3 (bank-process "c"))
                       (define bank-4 (bank-process "d"))
                       (define bank-5 (bank-process "e"))]
                    (start-debug first (list bank-1 bank-2 bank-3 bank-4 bank-5))))
;(bank)


(define (sum-snaps results) (cond [(empty? results) 0]
                                  [(cons? results) (+ (bank-state-snapshot (second (first results)))
                                                      (sum-snaps (rest results)))]))

(define (snapshot-correct? out)
  (= (sum-snaps out) (* STARTBAL (length out))))


;(snapshot-correct? (bank))
;; part p2b