#lang rosette
(require rosette/lib/synthax)


(define-synthax (alang depth)
  #:base (choose (?? integer?) '(r 1) '(r 2) '(r 3) '(r 4))
  #:else (choose (?? integer?) '(r 1) '(r 2) '(r 3) '(r 4)
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
    [else expr]
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
                 [else (error "cou;d not mathc")]
                 )
               )]
               [feed
                (lambda (output astate vstate stream)
                  (if (empty? stream)
                      vstate
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

(define (mlistappend data vstate)
  (if (first vstate)
      (list true (append (second vstate) (list data)))
      (list false vstate)
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

(define m (stransducer a stateful-append (list->vector '(0 0)) stateful-match
            [a : ('(1 #t) / (1 ((+ (r 1) 1))) / 1 -> b)]
            [b : ('(2 #t) / (1 (r 0)) / 1 -> a)]
            )
)

;(define ex0 (stransducer a stateful-append (list->vector '(4 4)) stateful-match
;            [a : ('(1 (<= (r 1) 100)) / (1 ((+ (r 1) (r 1)))) / 1 -> a)]
;            [a : ('(1 (<= 100 (r 1))) / (1 (r 2)) / 1 -> b)]
;            [b : ('(1 #t) / #t / 1 -> b)]
;            )
;)
;
;(define ex1 (stransducer a stateful-append (list->vector '(4 4)) stateful-match
;            [a : ('(1 (<= (r 1) 100)) / (1 ahole1) / 1 -> a)]
;            [a : ('(1 (<= 100 (r 1))) / (1 (r 0)) / 1 -> b)]
;            [b : ('(1 #t) / #t / 1 -> b)]
;            )
;)
;
;(define ex2 (stransducer a stateful-append (list->vector '(0 0)) stateful-match
;            [a : ('(1 bhole1) / (1 ahole1) / 2 -> a)]
;            [a : ('(1 bhole2) / (1 ahole2) / 2 -> a)]
;            [b : ('(1 #t) / #t / 2 -> b)]
;            )
;)
;
;(define ex3 (stransducer a stateful-append (list->vector '(4 4)) stateful-match
;            [a :
;               ('(1 bhole1) / (1 (+ (r 1) 1)) / 2 -> a)
;               ('(1 bhole2) / (0 ahole1) / 2 -> b)]
;            [b : ('(1 #t) / #t / 2 -> b)]
;            )
;)
;
;
;
;(define sol
;  (synthesize
;   #:forall (list)
;   #:guarantee (assert
;                (and 
;                     (equal? (ex3 '(1)) (list->vector '(4 12)))
;                     (equal? (ex3 '(1 1)) (list->vector '(4 36)))
;                     )
;   )
;  )
;  )
;
;(define sol2
;  (synthesize
;   #:forall (list)
;   #:guarantee (assert
;                (and 
;                     (equal? (ex1 '(1)) (list->vector '(4 8)))
;                     (equal? (ex1 '(1 1)) (list->vector '(4 12)))
;                     )
;   )
;  )
;  )
;
;(define (repeat n x)
;  (if (= 0 n)
;      '()
;      (cons x (repeat (- n 1) x))
;      )
;  )

;(define sol3
;  (synthesize
;   #:forall (list)
;   #:guarantee (assert
;                (and
;                 (equal? (ex2 '(1 1)) (list->vector '(0 2)))
;                 (equal? (ex2 '(1 1 1 1 1)) (list->vector '(0 0)))
;                 )
;                )
;   )
;  )
              
  
