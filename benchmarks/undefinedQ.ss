#lang rosette
(require rosette/lib/synthax)
(require "../stransducer.ss")

;;; seems to be a problem with one hole appear multiple times in the problem
; r1 tmin, r2 tmax, r3 tnow
(define TrafficController (stransducer a stateful-append-mult (list->vector '(0 0 0 3)) stateful-match
            [a :
               ('(1 (> (r 3) (r 2))) / [(1 (+ (r 3) (* 3 ahole1)))
                                          (2 (+ (r 3) ahole1))
                                          (3 (+ (r 3) 1))] / 2 -> a)
               ('(1 (and (< (r 1) (r 3)) (<= (r 3) (r 2)))) / [(1 (+ (r 1) ahole1))
                                                               (2 (+ (r 2) ahole1))
                                                               (3 (+ (r 3) 1))] / 2 -> a)
               ('(1 (<= (r 3) (r 1))) / [(3 (+ (r 3) 1))] / 1 -> a)]
            )
  )

(define sol
  (synthesize
   #:forall (list)
   #:guarantee (assert (equal? (TrafficController '(1 1 1)) '(2 1 1)))
   )
  )

(print-forms sol)