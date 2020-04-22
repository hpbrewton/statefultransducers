#lang rosette
(require rosette/lib/synthax)

(provide 
  stransducer
  stateful-append-mult
  stateful-match
  )

(define-synthax (alang depth)
  #:base (choose (?? integer?) '(r 1) '(r 2) '(r 3) '(r 4))
  #:else (choose (alang (- depth 1))
                   (list '+ (alang (- depth 1)) (alang (- depth 1)))
                   (list '* (alang (- depth 1)) (alang (- depth 1)))
                 )
  )

(define-synthax (blang depth)
  #:base (choose (?? boolean?))
  #:else (choose (?? boolean?)
                 (list '<= (alang (- depth 1)) (alang (- depth 1)))
                 (list '== (alang (- depth 1)) (alang (- depth 1)))
                 (list 'not (blang (- depth 1)))
                 (list 'or (blang (- depth 1)) (blang (- depth 1)))
                 (list 'and (blang (- depth 1)) (blang (- depth 1)))
                 )
  )

(define-symbolic a integer?)
(define-symbolic b integer?)
(define-symbolic x boolean?)
(define-symbolic y boolean?)
(define (leval vector expr)
  (if (list? expr)
  (case (first expr)
    ['and (apply && (map (lambda (e) (leval vector e)) (rest expr)))]
    ['or (apply || (map (lambda (e) (leval vector e)) (rest expr)))]
    ['not (not (leval vector (second expr)))]
    ['== (equal? (leval vector (second expr)) (leval vector (third expr)))]
    ['<= (<= (leval vector (second expr)) (leval vector (third expr)))]
    ['< (< (leval vector (second expr)) (leval vector (third expr)))]
    ['> (> (leval vector (second expr)) (leval vector (third expr)))]
    ['+ (apply + (map (lambda (e) (leval vector e)) (rest expr)))]
    ['* (apply * (map (lambda (e) (leval vector e)) (rest expr)))]
    ['r (vector-ref vector (second expr))]
    [else (leval vector (first expr))]
        )
  (case expr
    ['bhole1 (leval vector (blang 2))]
    ['bhole2 (leval vector (blang 2))]
    ['ahole1 (leval vector (alang 2))]
    ['ahole2 (leval vector (alang 2))]
    [else (if (number? expr)
      expr 
      (error "unexpected val")
    )]
  )
  )
)
(define v (list->vector '(1)))

(define-syntax stransducer
   (syntax-rules (: -> /)
     [(_ init-state vmap vid vmatch [state : (guard / data / out -> target) ...] ...)
      (letrec ([step
             ; astate is the automata state
             ; vstate is the vector state
             ; symbol is the current symbol under questions
             (lambda (output astate vstate symbol)
               (case astate
                   [(state) (cond
                            [(vmatch guard symbol vstate) (list (cons out output) 'target (vmap 'data vstate))] ...
                            [else  ((error (list output astate vstate)))]
                            )
                          ] ...
                 [else (error "could not match")]
                 )
               )]
               [feed
                (lambda (output astate vstate stream)
                  (if (empty? stream)
                      (reverse output)
                      (let ([result (step output astate vstate (first stream))])
                        (feed (first result) (second result) (third result) (rest stream))
                      )
                  )
                  )
                ])
        (lambda (stream) (feed '() 'init-state vid stream))
        )
      ]
     )
  )

(define (stateful-append-mult data vstate)
  (begin
    (foldr stateful-append vstate data)
    )
  )

(define (stateful-append data vstate)
  (begin
    (vector-set! vstate (first data) (leval vstate (second data)))
    vstate
    )
  )

(define (stateful-match g s v)
  (and
   (equal? (first g) s)
   (leval v (second g))
   )
  )

(define (match g s v)  
  (equal? g s)
  )

;;; ; r1 tmin, r2 tmax, r3 tnow
;;; (define B 3)
;;; (define Q 25)
;;; (define TrafficController (stransducer a stateful-append-mult (list->vector '(0 0 0 3)) stateful-match
;;;             [a :
;;;                ('(1 (> (r 3) (r 2))) / [(1 (+ (r 3) (* ahole1 25)))
;;;                                           (2 (+ (r 3) ahole2))
;;;                                           (3 (+ (r 3) 1))] / 2 -> a)
;;;                ('(1 (and (< (r 1) (r 3)) (<= (r 3) (r 2)))) / [(1 (+ (r 1) 25))
;;;                                                                (2 (+ (r 2) 25))
;;;                                                                (3 (+ (r 3) 1))] / 2 -> a)
;;;                ('(1 (<= (r 3) (r 1))) / [(3 (+ (r 3) 1))] / 1 -> a)]
;;;             )
;;;   )

;;; (define sol
;;;   (synthesize
;;;    #:forall (list)
;;;    #:guarantee (assert (equal? (TrafficController '(1 1 1)) '(2 2 1)))
;;;    )
;;;   )
                                                           
              
  
