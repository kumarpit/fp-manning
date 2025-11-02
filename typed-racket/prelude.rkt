#lang typed/racket
(require/typed rackunit
               [check-equal? (-> Any Any Void)]
               [check-exn (-> (-> Any Boolean) (-> Any) Void)])
(provide (all-defined-out))
(provide check-equal? check-exn)

;; Infinite computation, useful for testing
(: DIVERGE (-> Nothing))
(define (DIVERGE) (DIVERGE))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Option
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type (Option A) (U (Some A) (None A)))
(struct (A) None ())
(struct (A) Some ([value : A]))

(: option/get (All (A) (-> (Option A) A)))
(define (option/get opt)
  (match opt
    [(None) (error "Cannot get on an None option")]
    [(Some v) v]))

(: option-get-or-else (All (A) (-> (Option A) A A)))
(define (option-get-or-else opt default)
  (match opt
    [(None) default]
    [(Some v) v]))

(define option/map : (All (A B) (-> (Option A) (-> A B) (Option B)))
  (λ (opt f)
    (match opt
      [(None) (None)]
      [(Some v) (Some (f v))])))