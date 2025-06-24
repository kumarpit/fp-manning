#lang typed/racket
(require rackunit)
(require/typed rackunit
               [check-equal? (-> Any Any Void)]
               [check-exn (-> (-> Any Boolean) (-> Any) Void)])

;; Chapter 3: Functional Data Structures

(define-type (Tree α) (U (Leaf α) (Branch α)))
(struct (α) Leaf ([value : α]))
(struct (α) Branch ([left : (Tree α)] [right : (Tree α)]))


;; Ex 3.25 Implement a function size that counts the number of nodes in the tree
;; i.e both leaves and branches

(define tree/size : (All (α) (-> (Tree α) Integer))
  (λ (tree) (match tree
              [(Leaf _) 1]
              [(Branch tree-l tree-r)
               (+ 1 (tree/size tree-l) (tree/size tree-r))])))

(check-equal? (tree/size (Leaf 1)) 1)
(check-equal? (tree/size (Branch (Branch (Leaf 1) (Leaf 1))
                                 (Branch (Leaf 1) (Leaf 1))))
              7)

;; TODO: CPS this!

;; Ex 3.26 Write a function that returns the maximum leaf value in a tree

;; TODO: CPS this!

(define tree/max : (-> (Tree Integer) Integer)
  (λ (tree)
    (match tree
      [(Leaf value) value]
      [(Branch tree-l tree-r)
       (let* ([max-l (tree/max tree-l)]
              [max-r (tree/max tree-r)])
         (max max-l max-r))])))

(check-equal? (tree/max (Branch (Branch (Leaf 1) (Leaf -1))
                                (Branch (Leaf 8) (Leaf 2))))
              8)