#lang typed/racket
(require rackunit)
(require/typed rackunit
               [check-equal? (-> Any Any Void)]
               [check-exn (-> (-> Any Boolean) (-> Any) Void)])

;; Chapter 11: Monads

;; typed/racke doesn't have support for higher-kinded types, meaning
;; we cannot define a truly polymorphic Monad struct that works over any
;; type constructor like Listof, Option, or Boxof.
;;
;; For example, we might want to define a generic monad interface like:
;;
;;   struct Monad {
;;     return : (All (A) A -> (M A)),
;;     bind   : (All (A B) (M A) -> (A -> (M B)) -> (M B))
;;   }
;;
;; But Typed Racket does not allow us to abstract over the type constructor M,
;; only over fully-applied types like (Listof Number) or (Option Boolean).
;;
;; So we must define separate bind/return functions for each monadic type
;; without a shared abstraction.

;; Ex 11.1 Write Monad instances for Option, Stream, and List

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Option Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: extract the common utility data types to their own modules

(define-type (Option A) (U (Some A) (None A)))
(struct (A) None ())
(struct (A) Some ([value : A]))

(struct Option/Monad ([unit : (All (A) (-> A (Option A)))]
                      [flatmap : (All (A B) (-> (Option A)
                                                (-> A (Option B))
                                                (Option B)))]))

(define option/monad : Option/Monad
  (Option/Monad
   (λ (a) (Some a)) ; unit
   (λ (opt f) ; flatmap
     (match opt
       [(None) (None)]
       [(Some v) (f v)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct List/Monad ([unit : (All (A) (-> A (Listof A)))]
                    [flatmap : (All (A B) (-> (Listof A)
                                              (-> A (Listof B))
                                              (Listof B)))]))

(define list/monad : List/Monad
  (List/Monad
   (λ (a) (list a)) ; unit
   (λ (lst f) (append-map f lst)))) ; flatmap