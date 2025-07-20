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


(check-equal? (option/get (option/sequence (list (Some 1) (Some 2) (Some 3))))
              (list 1 2 3))
(check-exn exn:fail?
           (λ ()
             (option/get (option/sequence (list (Some 1) (Some 2) (None))))))
              
(define option/traverse :
  (All (A B) (-> (Listof A) (-> A (Option B)) (Option (Listof B))))
  (λ (lst f)
    (option/sequence (map (λ (a) (f a)) lst))))

(check-equal? (option/get (option/traverse (list 1 2 3) (λ (x) (Some x))))
              (list 1 2 3))
(check-exn exn:fail?
           (λ ()
             (option/get
              (option/traverse
               (list 1 2 3)
               (λ (x) (match x
                        [1 (None)]
                        [_ (Some x)]))))))

;; Ex 11.4 Implement replicateM for Monads
;; NOTE: Again, just going to be implementing these for Option/Monad

(define replicate-m :
  (All (A) (-> Integer (Option A) (Option (Listof A))))
  (λ (n opt)
    (option/sequence (build-list n (λ (_) opt)))))

(check-equal? (option/get (replicate-m 3 (Some 1)))
              (list 1 1 1))
(check-exn exn:fail? (λ () (option/get (replicate-m 3 (None))))) 

;; Ex 11.6 Implement the function filterM -- it is like filter except that
;; instead of a function from A => Boolean, we have an A => F[Boolean]
;; (where F is a monadic type constructor)

(define option/filter :
  (All (A) (-> (Listof A) (-> A (Option Boolean)) (Option (Listof A))))
  (λ (lst f)
    (foldr (λ ([a : A] [acc : (Option (Listof A))])
             ((Option/Monad-flatmap option/monad) ; can fail this computation
              (f a)
              (λ (bool)
                (if bool
                    (option/map acc (λ ([lst : (Listof A)]) (cons a lst)))
                    acc))))
           ((Option/Monad-unit option/monad) empty)
           lst)))

(check-equal? (option/get (option/filter (list 1 2 3) (λ ([x : Integer])
                                                        (Some (even? x)))))
              (list 2))
(check-exn exn:fail? (λ () (option/get
                            (option/filter (list 1 2 3 -1)
                                           (λ ([x : Integer])
                                             (if (< x 0)
                                                 (None)
                                                 (Some (even? x))))))))

;; 11.4 Monad Laws
;; Every Monad is a functor, so by definition they obey the Functor laws.
;; Why is a Monad a functor? Because you can implement fmap using unit and
;; flatmap (map == M.flatmap(λ (v) (unit (f v))))
;;
;; - The Associative Law
;; x.flatMap(f)).flatMap(g) == x.flatMap(a => f(a).flatMap(g)
;; It is not super clear that the law above is an associative law. The law can
;; be made clearer if we use Kleisli arrows (i.e monadic functions of type
;; A => F[B]).

;; Ex 11.7 Implement Kleisli composition

(define option/kleisli-compose :
  (All (A B C) (-> (-> A (Option B)) (-> B (Option C)) (-> A (Option C))))
  (λ (f g)
    (λ ([a : A]) : (Option C)
      ((Option/Monad-flatmap option/monad)
       (f a) g))))

;; Now we can state the monad associativity law more clearly as:
;; (compose (compose f g) h) == (compose f (compose g h))

;; Ex 11.8 Implement flatmap in terms of kleisli-compose

(define option/kleisli-flatmap :
  (All (A B) (-> (Option A) (-> A (Option B)) (Option B)))
  (λ (opt f)
    (((inst option/kleisli-compose Any A B)
      (λ (_) opt)
      f) 'ignored)))
