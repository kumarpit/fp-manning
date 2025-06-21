#lang racket
(require rackunit)

;; Functional Data Structures

;; Implementations for foldleft and foldright
(define (foldleft f acc lst)
  (match lst
    ['() acc]
    [(cons x xs) (foldleft f (f x acc) xs)]))

;; Not tail-recursive!
(define (foldright f z lst)
  (match lst
    ['() z]
    [(cons x xs) (f x (foldright f z xs))]))

(check-equal? (* 1 2 3) (foldleft (λ (x acc) (* x acc)) 1 (list 1 2 3)))
(check-equal? (+ 1 2 3) (foldleft (λ (x acc) (+ x acc)) 0 (list 1 2 3)))
(check-equal? (* 1 2 3) (foldright (λ (x acc) (* x acc)) 1 (list 1 2 3)))
(check-equal? (+ 1 2 3) (foldright (λ (x acc) (+ x acc)) 0 (list 1 2 3)))

;; Ex 3.12: Write a function to reverse a list
(define (reverse lst)
  (foldleft (λ (x acc) (cons x acc)) empty lst))

(check-equal? (reverse '(1 2 3)) '(3 2 1))
(check-equal? (reverse '(a b c)) '(c b a))
(check-equal? (reverse '()) '())
(check-equal? (reverse '(x)) '(x))

;; Ex 3.13: Can you write foldleft in terms of foldright? How about the other
;;          way around?

;; The key idea here is that a foldleft on a reversed list is logically
;; equivalent to foldright on the original list, and vice versa
(define (foldright-via-foldleft1 f z lst)
  (let ([rev-lst (reverse lst)])
    (foldleft f z rev-lst)))

(define (foldleft-via-foldright1 f z lst)
  (let ([rev-lst (reverse lst)])
    (foldright f z rev-lst)))

(check-equal? (* 1 2 3)
              (foldleft-via-foldright1 (λ (x acc) (* x acc)) 1 (list 1 2 3)))
(check-equal? (+ 1 2 3)
              (foldleft-via-foldright1 (λ (x acc) (+ x acc)) 0 (list 1 2 3)))
(check-equal? (* 1 2 3)
              (foldright-via-foldleft1 (λ (x acc) (* x acc)) 1 (list 1 2 3)))
(check-equal? (+ 1 2 3)
              (foldright-via-foldleft1 (λ (x acc) (+ x acc)) 0 (list 1 2 3)))

;; Using folds to build a composition of functions of the expected shape

(define (foldright-via-foldleft2 f z lst)
  ((foldleft
    (λ (x acc) (λ (b) (acc (f x b)))) identity lst) z)); (f x1 (f x2 b))
                
(define (foldleft-via-foldright2 f z lst)
  ((foldright (λ (x g) (λ (b) (g (f b x)))) identity lst) z)) ; (f (f b x2) x1)

(check-equal? (* 1 2 3)
              (foldleft-via-foldright2 (λ (x acc) (* x acc)) 1 (list 1 2 3)))
(check-equal? (+ 1 2 3)
              (foldleft-via-foldright2 (λ (x acc) (+ x acc)) 0 (list 1 2 3)))
(check-equal? (* 1 2 3)
              (foldright-via-foldleft2 (λ (x acc) (* x acc)) 1 (list 1 2 3)))
(check-equal? (+ 1 2 3)
              (foldright-via-foldleft2 (λ (x acc) (+ x acc)) 0 (list 1 2 3)))

;; Ex 3.14 Implement `append` in terms of either foldleft or foldright

(define (append/foldr x lst)
  (foldright (λ (x g) (cons x g)) (list x) lst))

(define (append/foldl x lst)
  ((foldleft (λ (x acc) (λ (b) (acc (cons x b)))) identity lst) (list x)))

(check-equal? (list 1 2 3 4) (append/foldr 4 (list 1 2 3)))
(check-equal? (list 1 2 3 4) (append/foldl 4 (list 1 2 3)))


;; Ex 3.15 Write a function that concatenates a list of lists into a single list

;; Concatenates two lists
(define (concat lst1 lst2)
  (foldright (λ (x g) (cons x g)) lst2 lst1))

(check-equal? (list 1 2 3 4) (concat (list 1 2) (list 3 4)))
(check-equal? (list 1 2 3 4) (concat (list 1 2 3 4) empty))

;; Flatten a list of lists
(define (flatten lsts)
  (foldright (λ (x g) (concat x g)) empty lsts))

(check-equal? (list 1 2 3 4 5 6 7 8)
              (flatten (list (list 1 2 3) (list 4) (list 5 6 7 8))))

;; Ex 3.18 Implement `map`
(define (map f lst)
  (foldright-via-foldleft2 (λ (x g) (cons (f x) g)) empty lst))

(check-equal? (list 1 2 3 4) (map add1 (list 0 1 2 3)))

;; Ex 3.19 Implement `filter`
(define (filter pred lst)
  (foldright-via-foldleft2 (λ (x g) (if (pred x) (cons x g) g)) empty lst))

(check-equal? (list 2 4) (filter even? (list 1 2 3 4)))

;; Ex 3.20 Implement `flatmap`
(define (flatmap f lst)
  (flatten (foldright-via-foldleft2 (λ (x g) (cons (f x) g)) empty lst)))

(check-equal? (list 1 1 2 2 3 3) (flatmap (λ (x) (list x x)) (list 1 2 3)))

;; Ex 3.21 Implement `filter` using `flatmap`
(define (filter-via-flatmap pred lst)
  (flatmap (λ (x) (if (pred x) (list x) empty)) lst))

(check-equal? (list 2 4) (filter-via-flatmap even? (list 1 2 3 4)))

;; Ex 3.22 Implement `zipwith`
(define (zipwith f lst1 lst2)
  (match (list lst1 lst2)
    [(list '() _) '()]
    [(list _ '()) '()]
    [(list (cons x xs) (cons y ys)) (cons (f x y) (zipwith f xs ys))]))

(check-equal? (list 5 7 9)
              (zipwith (λ (x y) (+ x y)) (list 1 2 3) (list 4 5 6)))


;; Ex 3.24 Implement `hasSubsequence`

;; Okay, one approach could be to zip the lists together -- once starting from
;; each element in the first list.

(define (has-subsequence lst1 lst2)
  (match lst1
    ['() (equal? lst1 lst2)]
    [_ #:when (< (length lst1) (length lst2)) #f] ;; length is an O(n) operation
    [(cons _ xs) (let ([zipped (zipwith (λ (x y) (equal? x y)) lst1 lst2)])
                   (if (foldleft (λ (x acc) (and x acc)) #t zipped)
                       #t
                       (has-subsequence xs lst2)))]))

;; This approach has a few issues. Firstly, we incur a linear cost on each
;; recursive call since we use length. The total costs of this is O(n^2)

(check-equal? (has-subsequence (list 1 2 3 4) (list 2 3 4)) #t)
(check-equal? (has-subsequence (list 1 2 3 4) (list 1 2))   #t)
(check-equal? (has-subsequence (list 1 2 3 4) (list 3 4))   #t)
(check-equal? (has-subsequence (list 1 2 3 4) (list 4))     #t)
(check-equal? (has-subsequence (list 1) (list 1 2 3))       #f)
(check-equal? (has-subsequence '() (list 1 2 3))            #f)
(check-equal? (has-subsequence '() '())                     #t)
(check-equal? (has-subsequence (list 1 2 3 4) (list 5 6 7)) #f)