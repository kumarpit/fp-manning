#lang typed/racket
(require "prelude.rkt")

;; Chapter 10: Monoids

;; A monoid is a "purely algebraic structure". This means that it is only
;; defined by its algebra, and instances of monoids may little to do with each
;; other apart from sharing the same algebra (laws). A monoid consists of the
;; following:
;; - Some type A
;; - A *associative* binary operation that takes two values of type A and
;;   combines them into one s.t (equal? (op x (op y z)) (op (op x y) z)) for any
;;   x, y, z
;; - An identity element (zero) s.t (equal? (op x zero) (op zero x) x)

(struct (A) Monoid ([zero : A] [combine : (-> A A A)]))

;; Example of a monoid for A = String
(define monoid/string : (Monoid String)
  (Monoid "" (λ ([a : String] [b : String]) (string-append a b))))

;; Ex 10.1 Give monoid instances for integer addition, multiplication, as well
;; as the Boolean operators

(define monoid/int-add : (Monoid Integer)
  (Monoid 0 (λ ([a : Integer] [b : Integer]) : Integer (+ a b))))

(define monoid/int-mult : (Monoid Integer)
  (Monoid 1 (λ ([a : Integer] [b : Integer]) : Integer (* a b))))

(define monoid/bool-or : (Monoid Boolean)
  (Monoid false (λ ([a : Boolean] [b : Boolean]) : Boolean (or a b))))

(define monoid/bool-and : (Monoid Boolean)
  (Monoid true (λ ([a : Boolean] [b : Boolean]) : Boolean (and a b))))

;; Ex 10.2 Give the monoid instance for combining Option values

(define monoid/opt : (All (A) (-> (Monoid (Option A))))
  (λ () ((inst Monoid (Option A)) (None) (λ ([a : (Option A)] [b : (Option A)])
                                           (match a
                                             [(None) b]
                                             [_ a])))))

;; A function having the same argument and return type is sometimes called an
;; *endofunction*

;; Ex 10.3 Write a monoid for endofunctions
#;
(define-type Endofun (All (A) (-> A A)))

(define-type (Endofun A) (-> A A))

;; There is a subtle issue with this definition - mainly, A is *NOT* the same
;; each Endofun instance here
#;
(define monoid/endofun : (-> (Monoid Endofun))
  (λ () ((inst Monoid Endofun)
         identity
         (λ ([a : Endofun] [b : Endofun]) (λ (c) (b (a c)))))))

(define monoid/endofun : (All (A) (-> (Monoid (Endofun A))))
  (λ () ((inst Monoid (Endofun A))
         identity
         (λ ([a : (Endofun A)] [b : (Endofun A)]) : (Endofun A)
           (λ (v) (b (a v))))))) ; Could also compose as (a (b v))


;; Monoids have an intimate connection with lists

(define space/string " ")
(define list/strings (list "I"
                           space/string
                           "am"
                           space/string
                           "a"
                           space/string
                           "list"))

;; You would expect this to produce the same output as with a foldl, but that is
;; not the case since Racket's foldl/foldr have the property that:
;; (foldr cons null lst) = lst
;; (foldl cons null lst) = (reverse lst)
;; See https://stackoverflow.com/questions/8778492
(foldr (Monoid-combine monoid/string)
       (Monoid-zero monoid/string)
       list/strings)

;; Also note that each monoid has a "dual" that can be achieved by flipping the
;; arguments to the combine function. This is because the combine function need
;; not be commutative.

(define monoid/dual : (All (A) (-> (Monoid A) (Monoid A)))
  (λ (m)
    (Monoid
     (Monoid-zero m)
     (λ ([a : A] [b : A]) ((Monoid-combine m) b a)))))

;; Ex 10.5 Implement foldMap

(define list/foldmap : (All (A B) (-> (-> A B) (Monoid B) (Listof A) B))
  (λ (f m lst)
    (foldr (λ ([x : A] [acc : B]) ((Monoid-combine m) (f x) acc))
           (Monoid-zero m)
           lst)))


;; Ex 10.6 The foldMap function can be implemented using foldRight or foldLeft,
;; but you can also implement foldLeft/foldRight using foldMap. Try it.

;; The idea here is to notice that the function passed to foldright contains an
;; Endofun! (A, B) -> B == A -> B -> B
;; So we first each element of the list to get a list of B -> B
;; Then compose these endo-functions in the right order, depending on whether we
;; we want left/right associativity

(define foldright/foldmap : (All (A B) (-> (-> A (Endofun B)) B (Listof A) B))
  (λ (f acc lst)
    (((inst list/foldmap A (Endofun B))
      (λ ([a : A]) (f a))
      ((inst monoid/dual (Endofun B)) (monoid/endofun)) ;; The default Endofun
      ;; monoid in my implementation is left associative
      lst)
     acc)))

;; Same as above, except we use the original endofun monoid rather than its dual
(define foldleft/foldmap : (All (A B) (-> (-> A (Endofun B)) B (Listof A) B))
  (λ (f acc lst)
    (((inst list/foldmap A (Endofun B))
      (λ ([a : A]) (f a))
      (monoid/endofun) ;;
      lst)
     acc)))


;; 10.6 Composing Monoids

;; The Monoid abstraction in itself is not all that compelling, and with the
;; generalized `foldmap` it's only slightly more interesting. The real power
;; of monoids comes from the fact that they compose.
;;
;; This means that, if types A and B are monoids, then the tuple types (A, B)
;; is also a monoid (called their product).

;; Ex 10.16 Prove it

;; Well, we need to show that there exists an identity element and an
;; associative operation for the tuple (A, B).

(define monoid/product : (All (A B)
                              (-> (Monoid A) (Monoid B) (Monoid (Vector A B))))
  (λ (ma mb)
    ((inst Monoid (Vector A B))
     (vector
      (Monoid-zero ma)
      (Monoid-zero mb))
     (λ ([a : (Vector A B)] [b : (Vector A B)])
       (vector
        ((Monoid-combine ma) (vector-ref a 0) (vector-ref b 0))
        ((Monoid-combine mb) (vector-ref a 1) (vector-ref b 1)))))))


;; Containers that contain monoid types form really interesting monoids!
;; As an example, consider this monoid for merging key-value Maps as long as the
;; value type is a monoid.

(define hashtable/monoid/merge : (All (K V)
                                      (-> (Monoid V)
                                          (Monoid (Mutable-HashTable K V))))
  (λ (vm)
    (let: [(zero : (Mutable-HashTable K V) (make-hash))]
      ((inst Monoid (Mutable-HashTable K V))
       zero
       (λ ([a : (Mutable-HashTable K V)] [b : (Mutable-HashTable K V)])
         (foldl
          (λ ([curr : K] [acc : (Mutable-HashTable K V)])
            : (Mutable-HashTable K V)
            (begin
              (hash-set! acc curr
                         ((Monoid-combine vm)
                          ; Atleast one value is guranteed to exist
                          (hash-ref a curr (λ () (Monoid-zero vm)))
                          (hash-ref b curr (λ () (Monoid-zero vm)))))
              acc))
          zero
          (remove-duplicates (append (hash-keys a) (hash-keys b)))))))))


;; Example - MapMerge

(define-type MyHashTable
  (Mutable-HashTable Symbol (Mutable-HashTable Symbol Integer)))

(define myhashtable1 : MyHashTable
  (make-hash (list (cons 'o1
                         (make-hash (list
                                     (cons 'i1 1)
                                     (cons 'i2 2)))))))

(define myhashtable2 : MyHashTable
  (make-hash (list (cons 'o1
                         (make-hash (list
                                     (cons 'i2 3)))))))

(define myhashtable/monoid : (Monoid MyHashTable)
  ((inst hashtable/monoid/merge Symbol (Mutable-HashTable Symbol Integer))
   ((inst hashtable/monoid/merge Symbol Integer)
    monoid/int-add)))

(check-equal?
 ((Monoid-combine myhashtable/monoid) myhashtable1 myhashtable2)
 (make-hash (list (cons 'o1
                        (make-hash (list
                                    (cons 'i1 1)
                                    (cons 'i2 5)))))))

;; Ex 10.17 Write a monoid instance for functions whose results are monoids
(define monoid/func : (All (A B) (-> (Monoid B) (Monoid (-> A B))))
  (λ (mb)
    ((inst Monoid (-> A B))
     (λ ([_ : A]) (Monoid-zero mb))
     (λ ([a : (-> A B)] [b : (-> A B)])
       (λ ([arg : A])
         ((Monoid-combine mb)
          (a arg)
          (b arg)))))))

;; Ex 10.18 Implement bag* for a list
;; *bag is a mapping from element -> count

(define make-bag : (All (A) (-> (Listof A) (Mutable-HashTable A Integer)))
  (λ (lst)
    (list/foldmap
     (λ ([elem : A]) : (Mutable-HashTable A Integer)
       (make-hash (list (cons elem 1))))
     ((inst hashtable/monoid/merge A Integer) monoid/int-add)
     lst)))

(check-equal? (make-bag (list 'a 'b 'c 'a 'a 'b))
              (make-hash (list
                          (cons 'a 3)
                          (cons 'b 2)
                          (cons 'c 1))))

;; Using composed monoids to fuse traversals - i.e composed monoids allow us
;; to perform multiple calculations simulatenously when folding a datastructure

;; Example - Averaging values in a list
(check-equal? (list/foldmap
               (λ ([e : Integer]) (vector 1 e))
               (monoid/product monoid/int-add monoid/int-add)
               (list 1 2 3 4))
              (vector 4 10)) ; count, sum

;; While it is tedious to assemble monoids by hand by taking their product and
;; foldmapping, but this one can develop a combinator library that makes this
;; process more convenient and allows for parallelizing complex computations.

