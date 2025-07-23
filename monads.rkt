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

;; - The identity laws
;; Just like monoids had an identity element `zero`, monads have an identity
;; element for compose -- this is exactly what unit is, i.e.
;; (compose f unit) == f
;; (compose unit f) == f
;;
;; Stating these laws in terms of flatmap makes them a lot less clearer:
;; (flatmap x unit) == x
;; (flatmap (unit y) f) == f(y)

;; Ex 11.10 Prove that these two statements of the identity laws are equivalent
;; Rewriting the compose version in terms of flatmap, we get:
;; 1. (compose f unit) == (a => f(a).flatmap(unit))
;; 2. (compose unit f) == (a => unit(a).flatmap(f))
;; Rewriting, we get
;; 1. => (flatmap x unit) = x ; Replace f(a) with x
;; 2. => (flatmap (unit y) f) = f(y) ; Replace a with y

;; Ex 11.11 Prove that the identity laws hold for a Monad of your choice
;; Using Option
;; (1)
;; In the case that f returns None, 1. is trivially true since None always
;; compose to Nones
;; In the case that f return a Some value, it is clear that Some compose with
;; unit is logically equivalent to just calling f
;;
;; (2)
;; Again, in the case that f returns None, 2 is trivially true
;; In the case that f returns a Some value, it is easy to see that composition
;; with unit is equivalent to just calling f

;; Ex 11.12 Implement the `join` combinator
(define option/join :
  (All (A) (-> (Option (Option A)) (Option A)))
  (λ (opt-sqrd)
    ((inst (Option/Monad-flatmap option/monad) (Option A) A)
     opt-sqrd
     identity)))

;; Ex 11.13 Implement either flatmap or compose in terms of join and map
(define option/monad-flatmap2 :
  (All (A B) (-> (Option A) (-> A (Option B)) (Option B)))
  (λ (opt f)
    (option/join (option/map opt f))))

(define option/klesli-compose2 :
  (All (A B C) (-> (-> A (Option B)) (-> B (Option C)) (-> A (Option C))))
  (λ (f g)
    (λ ([a : A])
      (option/join (option/map (f a) g)))))

;; Ex 11.14 Restate the monad laws to only mention join, map, and unit
;; 1. Associative law
;; Translating the compose based version we get:
;; (join (map h (join (map g (f a)))))) == 
;; (join (map (λ (y) (join (map h (g y)))) (f a)))
;;
;; This can be simplified by trying to convert the flatmap version instead.
;; Since the associative law holds for all f and g, we can use the identity
;; for both to simplify our reasoning. This gives us:
;; x.flatMap(identity).flatMap(identity) == x.flatMap(a => a.flatMap(identity))
;;
;; And since flatmap and identity is a join (and a flatmap itself is a map and a
;; join), we get our result:
;; (join (join x)) == (join (map x join))
;;
;; 2. Identity law
;; (join (map x unit)) == x
;; (join (map (unit y) f)) == (f y)

;; Just what is a monad???
;; We can see that a chain of flatMap calls (or an equivalent
;; for-comprehension) is like an imperative program with statements that assign
;; to variables, and the monad specifies what happens at statement boundaries.
;; For example, the Identity monad does nothing but wrap/unwrap the values, the
;; Option monad provides the ability for a statement to return None and
;; terminate the program, a list Monad may return multiple values, which causes
;; statements that follow to possibly run multiple times, etc.
;; The Moand contract itself doesn't specify any of this "between the lines"
;; behaviour, just that whatever is happening between the lines satisfies the
;; laws of associativity and identity.