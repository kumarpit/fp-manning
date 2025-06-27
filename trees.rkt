#lang typed/racket
(require rackunit)
(require/typed rackunit
               [check-equal? (-> Any Any Void)]
               [check-exn (-> (-> Any Boolean) (-> Any) Void)])

;; Chapter 3: Functional Data Structures

;; This allows the possibility of having a (Branch (Empty) (Empty))
;; Probably better to just have define a node with value and optional
;; left/right branches.
(define-type (Tree α) (U (Leaf α) (Branch α) (Empty α)))
(struct (α) Leaf ([value : α]))
(struct (α) Branch ([left : (Tree α)] [right : (Tree α)]))
(struct (α) Empty ())


;; Ex 3.25 Implement a function size that counts the number of nodes in the tree
;; i.e both leaves and branches

(define tree/size : (All (α) (-> (Tree α) Integer))
  (λ (tree) (match tree
              [(Empty) 0]
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
      [(Empty) 0]
      [(Leaf value) value]
      [(Branch tree-l tree-r)
       (let* ([max-l (tree/max tree-l)]
              [max-r (tree/max tree-r)])
         (max max-l max-r))])))

(check-equal? (tree/max (Branch (Branch (Leaf 1) (Leaf -1))
                                (Branch (Leaf 8) (Leaf 2))))
              8)

;; Ex 3.27 Write a function depth that returns that maximum path length from the
;; root to any leaf

(define tree/depth : (All (α) (-> (Tree α) Integer))
  (λ (tree)
    (match tree
      [(Empty) 0]
      [(Leaf _) 1]
      [(Branch tree-l  tree-r)
       (let* ([max-l (tree/depth tree-l)]
              [max-r (tree/depth tree-r)])
         (+ 1 (max max-l max-r)))])))

(check-equal? (tree/depth (Branch (Branch (Leaf 1) (Leaf 1))
                                  (Branch (Empty)
                                          (Branch (Leaf 1) (Leaf 1)))))
              4)


;; Ex 3.28 Write map for trees

(define tree/map : (All (α β) (-> (-> α β) (Tree α) (Tree β)))
  (λ (f tree)
    (match tree
      [(Empty) (Empty)]
      [(Leaf v) (Leaf (f v))]
      [(Branch tree-l tree-r)
       (Branch (tree/map f tree-l) (tree/map f tree-r))])))

;; Ex 3.29 Generalize these operations into a fold

;; TODO!!!