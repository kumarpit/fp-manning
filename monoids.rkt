#lang typed/racket
(require rackunit)
(require/typed rackunit
               [check-equal? (-> Any Any Void)]
               [check-exn (-> (-> Any Boolean) (-> Any) Void)])

;; Chapter 7: Monoids
;; Related: Category Theory

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
  (λ () ((inst Monoid (Option A)) #f (λ ([a : (Option A)] [b : (Option A)])
                                       (match a
                                         [#f b]
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
           (λ (c) (b (a c))))))) ; Could also compose (a (b c))


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