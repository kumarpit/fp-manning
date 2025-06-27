#lang typed/racket
(require rackunit)
(require/typed rackunit
 [check-equal? (-> Any Any Void)]
 [check-exn (-> (-> Any Boolean) (-> Any) Void)])

;; Chapter 7: Monoids

;; A monoid is a "purely algebraic structure". This means that it is only
;; defined by its algebra, and instances of monoids may little to do with each
;; other apart from sharing the same algebra (laws). A monoid consists of the
;; following:
;; - Some type A
;; - A *associative* binary operation that takes two values of type A and
;;   combines them into one s.t (equal? (op x (op y z)) (op (op x y) z)) for any
;;   x, y, z
;; - An identity element (zero) s.t (equal? (op x zero) (op zero x) x)

