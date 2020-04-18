#lang rosette

(require synapse/opsyn/engine/metasketch
         synapse/opsyn/metasketches/superoptimization
         synapse/opsyn/metasketches/cost
         synapse/opsyn/lang)

(define (max-post P inputs)
  (match-define (list x y) inputs)
  (define out (interpret P inputs)
    (assert (>= out x))
    (assert (>= out y))
    (assert (or (= out x) (= out y)))))

(define (example)
  (support∑ #:arity 2
            #:instructructions (list bvslt ite)
            #:post max-post
            #:cost-model constant-cost-model))
