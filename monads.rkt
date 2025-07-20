#lang typed/racket
(require "prelude.rkt")
(require "streams.rkt")

;; Chapter 11: Monads

;; typed/racket doesn't have support for higher-kinded types, meaning
;; we cannot define a truly polymorphic Monad struct that works over any
;; type constructor like Listof, Option, or Boxof.
;;
;; For example, we might want to define a generic monad interface like:
;;
;;   struct Monad {
;;     unit    : (All (A) A -> (M A)),
;;     flatmap : (All (A B) (M A) -> (A -> (M B)) -> (M B))
;;   }
;;
;; But Typed Racket does not allow us to abstract over the type constructor M,
;; only over fully-applied types like (Listof Number) or (Option Boolean).
;;
;; So we must define separate unit/flatmap functions for each monadic type
;; without a shared abstraction.

;; Ex 11.1 Write Monad instances for Option, Stream, and List

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Option Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stream Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct Stream/Monad ([unit : (All (A) (-> A (Stream A)))]
                      [flatmap : (All (A B) (-> (Stream A)
                                                (-> A (Stream B))
                                                (Stream B)))]))

(define stream/monad : Stream/Monad
  (Stream/Monad
   (λ (a) (list->stream (list a))) ; unit
   (λ (stream f) (stream-flatmap stream f)))) ; flatmap

;; Ex 11.3 Implement the sequence and traverse combinators for Monads
;; NOTE: Just implementing this for Option/Monad, should be the same for other
;;       types.

;; Short-circuit if any element in the list is None
(define option/sequence : (All (A) (-> (Listof (Option A)) (Option (Listof A))))
  (λ (opt-lst)
    (foldr (λ ([elem : (Option A)] [acc : (Option (Listof A))])
             ((Option/Monad-flatmap option/monad)
              elem
              (λ ([v : A])
                (option/map acc (λ ([lst : (Listof A)]) (cons v lst))))))
           ((Option/Monad-unit option/monad) empty)
           opt-lst)))

(define option/traverse :
  (All (A B) (-> (Listof A) (-> A (Option B)) (Option (Listof B))))
  (λ (lst f)
    (option/sequence (map (λ (a) (f a)) lst))))
