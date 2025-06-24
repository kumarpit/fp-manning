#lang typed/racket
(require rackunit)
(require/typed rackunit
 [check-equal? (-> Any Any Void)]
 [check-exn (-> (-> Any Boolean) (-> Any) Void)])


(define-type (Option A) (U (Some A) (None A)))
(struct (A) None ())
(struct (A) Some ([value : A]))

(: option-get (All (A) (-> (Option A) A)))
(define (option-get opt)
  (match opt
    [(None) (error "Cannot get on an None option")]
    [(Some v) v]))

(: option-get-or-else (All (A) (-> (Option A) A A)))
(define (option-get-or-else opt default)
  (match opt
    [(None) default]
    [(Some v) v]))

;; Chapter 5: Strictness and Laziness

;; Infinite computation, useful for testing
(: DIVERGE (-> Nothing))
(define (DIVERGE) (DIVERGE))

;; Reads (Promiseof A)
(struct (A) Promise ([thunk : (-> A)]))

;; Scheme-style promises (i.e in case of self-referential promise, returns the
;; *first* computed value - or diverge)
(: construct-promise (All (A) (-> (-> A) (Promise A))))
(define (construct-promise thunk)
  (let* ([result-ready? : Boolean #f]
         [result : (Option A) (None)])
    (Promise
     (λ () (let ([computed (thunk)])
             (if result-ready?
                 (match result
                   [(None) (error "This should never happen")]
                   [(Some value) value])
                 (begin
                   (set! result-ready? #t)
                   (set! result (Some computed))
                   computed)))))))

(: force (All (A) (-> (Promise A) A)))
(define (force p) ((Promise-thunk p)))
  
(define-syntax delay
  (syntax-rules ()
    [(_ expression)
     (construct-promise (λ () expression))]))

(check-equal? (Promise? (delay (DIVERGE))) #t)
(check-equal? (let ([p (delay (+ 9 7))])
                (force p))
              16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Homogenous streams
(define-type (Stream A) (U (Sempty A) (Scons A)))
(struct (A) Sempty ())
(struct (A) Scons ([head : (Promise A)] [tail : (Promise (Stream A))]))

(: Stream? (-> Any Boolean))
(define (Stream? v) (or (Sempty? v) (Scons? v)))

;; In the book, the authors talk about implementing a "smart" constructor for
;; stream cons that caches the result of the thunk so that it isn't evaluated
;; each time the stream is forced. This isn't required in this implementation
;; since our Promises are cached by design.

;; Converts regular lists to streams
(: list->stream (All (A) (-> (Listof A) (Stream A))))
(define (list->stream lst)
  (cond [(empty? lst) (Sempty)]
        [else (Scons (delay (first lst)) (delay (list->stream (rest lst))))]))


;; Ex 5.1 Write a function to convert a stream to a list
(: stream->list (All (A) (-> (Stream A) (Listof A))))
(define (stream->list stream)
  (match stream
    [(Sempty) '()]
    [(Scons hd tl) (cons (force hd) (stream->list (force tl)))]))
  
(check-equal? (Stream? (list->stream (list 1 2 3 4))) #t)
(check-equal? (list?
               (stream->list (Scons (delay 1)
                                    (delay (Scons (delay 2)
                                                  (delay (Sempty))))))) #t)

;; Ex 5.2 Implement take-n and drop-n for Stream
(: take-n (All (A) (-> (Stream A) Integer (Stream A))))
(define (take-n stream n)
  (cond [(<= n 0) (Sempty)]
        [else (match stream
                [(Sempty) (Sempty)]
                [(Scons hd tl) (Scons hd (delay (take-n (force tl)
                                                        (sub1 n))))])]))

(check-equal? (stream->list (take-n
                             (list->stream (list 1 2 3 4 5)) 2))
              (list 1 2))

(: drop-n (All (A) (-> (Stream A) Integer (Stream A))))
(define (drop-n stream n)
  (match stream
    [(Sempty) (Sempty)]
    [(Scons hd tl) (if (<= n 0) stream (drop-n (force tl) (sub1 n)))]))


;; Ex 5.3 Implement takeWhile for returning all starting elements in a stream
;; that match a given predicate
(: take-while (All (A) (-> (Stream A) (-> (Promise A) Boolean) (Stream A))))
(define (take-while stream pred)
  (match stream
    [(Sempty) (Sempty)]
    [(Scons hd tl) (if (pred hd)
                       (Scons hd (delay (take-while (force tl) pred)))
                       (Sempty))]))

(check-equal? (stream->list (take-while
                             (list->stream (list 2 4 6 7 8 9))
                             (λ ([x : (Promise Integer)]) (even? (force x)))))
              (list 2 4 6))

;; [5.3] Separating program description from evaluation

(: exists1 (All (A) (-> (Stream A) (-> A Boolean) Boolean)))
(define (exists1 stream pred)
  (match stream
    [(Sempty) #f]
    [(Scons hd tl) (if (pred (force hd)) #t (exists1 (force tl) pred))]))

(: foldright (All (A B) (-> (-> A (Promise B) B) (Promise B) (Stream A) B)))
(define (foldright f z stream)
  (match stream
    [(Sempty) (force z)]
    ;; If f never evaluates its second argument, recursion never occurs -- this
    ;; way, we can achieve early termination!
    [(Scons hd tl) (f (force hd) (delay (foldright f z (force tl))))]))

;; Since foldright can now support early termination, we can use it to implement
;; exists1. This isn't possible with the original (eager) foldright. So
;; laziness has made our code more reusable. Note that this still isn't stack
;; safe!
(define exists2 :
  (All (A) (-> (Stream A) (-> A Boolean) Boolean))
  (λ (stream pred)
    (foldright (λ ([a : A] [b : (Promise Boolean)]) : Boolean
                 (if (pred a) #t (force b)))
               (delay #f)
               stream)))

;; Ex 5.4 Implement forAll with early termination
(define forall :
  (All (A) (-> (Stream A) (-> A Boolean) Boolean))
  (λ (stream pred)
    (foldright (λ ([a : A] [b : (Promise Boolean)]) : Boolean
                 (and (pred a) (force b))) ;; and is lazy in 2nd arg
               (delay #t)
               stream)))

;; Ex 5.5 Use foldright to implement takewhile
(define takewhile2 :
  (All (A) (-> (Stream A) (-> A Boolean) (Stream A)))
  (λ (stream pred)
    (foldright (λ ([a : A] [b : (Promise (Stream A))]) : (Stream A)
                 (if (pred a) (Scons (delay a) b) (Sempty)))
               (delay (Sempty))
               stream)))

(check-equal? (stream->list (takewhile2
                             (list->stream (list 2 4 6 7 8 9))
                             even?))
              (list 2 4 6))

;; Ex 5.6 Implement headoption using foldright
(define headoption :
  (All (A) (-> (Stream A) (Option A)))
  (λ (stream)
    ;; never recurses
    (foldright (λ ([a : A] [_ : (Promise (Option A))]) : (Option A)
                 (Some a)) (delay (None)) stream)))

(check-equal? (option-get (headoption (list->stream (list 1 2 3)))) 1)
(check-exn exn:fail? (λ () (option-get (headoption (Sempty)))))

;; Ex 5.7 Implement map, filter, append, and flatmap using foldright
(define stream-map :
  (All (A B) (-> (Stream A) (-> A B) (Stream B)))
  (λ (stream f)
    (foldright (λ ([a : A] [b : (Promise (Stream B))]) : (Stream B)
                 (Scons (delay (f a)) b)) ;; The key point to notice
               ;; here is the (delay (f a))
               (delay (Sempty))
               stream)))

(check-equal? (stream->list (stream-map (list->stream (list 1 2 3))
                                        add1))
              (list 2 3 4))

(define stream-filter :
  (All (A) (-> (Stream A) (-> A Boolean) (Stream A)))
  (λ (stream pred)
    (foldright (λ ([a : A] [b : (Promise (Stream A))]) : (Stream A)
                 (if (pred a)
                     (Scons (delay a) b)
                     (force b)))
               (delay (Sempty))
               stream)))

(check-equal? (stream->list (stream-filter (list->stream (list 1 2 3))
                                           even?))
              (list 2))

(define stream-append :
  (All (A) (-> (Stream A) A (Stream A)))
  (λ (stream elem)
    (foldright (λ ([a : A] [b : (Promise (Stream A))]) : (Stream A)
                 (Scons (delay a) b))
               (delay (Scons (delay elem) (delay (Sempty))))
               stream)))

(check-equal? (stream->list
               (stream-append (list->stream (list 1 2 3 4)) 5))
              (list 1 2 3 4 5))

(define stream-concat :
  (All (A) (-> (Stream A) (Stream A) (Stream A)))
  (λ (stream1 stream2)
    (foldright (λ ([a : A] [b : (Promise (Stream A))]) : (Stream A)
                 (Scons (delay a) b))
               (delay stream2)
               stream1)))

(check-equal? (stream->list
               (stream-concat (list->stream (list 1 2 3 4))
                              (list->stream (list 5 6 7 8))))
              (list 1 2 3 4 5 6 7 8))

(define stream-flatmap :
  (All (A) (-> (Stream A) (-> A (Stream A)) (Stream A)))
  (λ (stream f)
    (foldright (λ ([a : A] [b : (Promise (Stream A))]) : (Stream A)
                 (stream-concat (f a) (force b)))
               (delay (Sempty))
               stream)))

(check-equal? (stream->list
               (stream-flatmap (list->stream (list 1 2 3))
                               (λ (x) (list->stream (list x x x)))))
              (list 1 1 1 2 2 2 3 3 3))
                              
;; Example demonstrating the "incremental" nature of stream operations
(stream->list (stream-filter
               (stream-map (list->stream (list 1 2 3 4 5 6))
                           (λ ([x : Integer]) : Integer
                             (begin
                               (println "mapping")
                               (add1 x))))
               (λ ([x : Integer]) : Boolean
                 (begin
                   (println "filtering")
                   (even? x)))))

;; Compare this to the same operations on a regular list
(filter
 (λ ([x : Integer]) : Boolean
   (begin (println "filtering") (even? x)))
 (map
  (λ ([x : Integer]) : Integer
    (begin (println "mapping") (add1 x)))
  (list 1 2 3 4 5 6)))

;; Since intermediate streams are not instantiated, we can use combinators
;; (which intuitively operate on the entire list) to perform short-circuiting
;; behaviours such as finding the first element in a stream that matches a
;; given predicate -- this can now be implemented using `stream-filter`

(define stream-find :
  (All (A) (-> (Stream A) (-> A Boolean) (Option A)))
  (λ (stream pred)
    (headoption (stream-filter stream pred)))) ;; even though we are filtering
;; we only ever compute until the
;; first element is found

;; [5.4] Infinite Streams and Corecursion

;; Because streams enable incremental computation, we can operate on infinite
;; lists without blowing the stack

(define ones : (Stream 1) (Scons (delay 1) (delay ones)))

;; 5 ones for you
(check-equal? (stream->list (take-n ones 5)) (list 1 1 1 1 1))

;; Ex 5.8 Generalize ones
(define constant : (All (A) (-> A (Stream A)))
  (λ (x) (Scons (delay x) (delay (constant x)))))

(check-equal? (stream->list (take-n (constant 'a) 5))
              (list 'a 'a 'a 'a 'a))


;; Ex 5.9 Write a function that generates an infinite stream of integers
;; starting from n
(define stream-from : (-> Integer (Stream Integer))
  (λ (n) (Scons (delay n) (delay (stream-from (add1 n))))))

(check-equal? (stream->list (take-n (stream-from 3) 3))
              (list 3 4 5))

;; Ex 5.10 Write the function fibs that generates the infinite stream of
;; fibonacci numbers
(define fibs : (-> (Stream Integer))
  (letrec ([fibs-helper : (-> Integer Integer (Stream Integer))
                        (λ (current next)
                          (Scons (delay current)
                                 (delay
                                   (fibs-helper next
                                                (+ current next)))))])
    (λ () (fibs-helper 0 1))))

(check-equal? (stream->list (take-n (fibs) 8)) (list 0 1 1 2 3 5 8 13))

;; Ex 5.11 Write a more general stream-building function called unfold. It
;; takes an initial state, and a function for producing both the next state,
;; and the next value in the generate stream.

(define unfold :
  (All (A B) (-> B (-> B (Option (Pair A B))) (Stream A)))
  (λ (initial-state f)
    (match (f initial-state)
      [(None) (Sempty)]
      [(Some (cons value next-state)) (Scons (delay value)
                                             (delay (unfold next-state f)))])))

;; The `Option` return type from the `f` function is used to indicate when the
;; stream should be terminated. The `unfold` function is an example of what's
;; sometimes called a "corecursive" function. A recursive function usually
;; consumes data (eg. list iteration), but a corecursive function produces data.
;; Corecursive functions need not terminate as long as they remain productive --
;; which just means we can compute more of the result in a finite amount of time
;; In this case, the unfold function remains productive as long as f terminates
;; Corecursion is sometimes called "guarded recursion" and productivity is
;; sometimes called "cotermination".

;; Ex 5.12 Implement fibs, constant, and ones in terms of unfold
;; *the unfold based versions do not preserve sharing
(define (unfold-fibs) (unfold (cons 0 1)
                              (λ ([state : (Pair Integer Integer)])
                                (let* ([current (car state)]
                                       [next (cdr state)])
                                  (Some (cons current
                                              (cons next
                                                    (+ current next))))))))

(check-equal? (stream->list (take-n (unfold-fibs) 8)) (list 0 1 1 2 3 5 8 13))

(define unfold-constant :
  (All (A) (-> A (Stream A)))
  (λ (x)
    (unfold x (λ ([v : A]) : (Option (Pair A A))
                (Some (cons v v))))))

(check-equal? (stream->list (take-n (unfold-constant 'a) 5))
              (list 'a 'a 'a 'a 'a))

(define unfold-ones :
  (-> (Stream 1))
  (λ () (unfold 1 (λ (_) (Some (cons 1 1))))))

(check-equal? (stream->list (take-n (unfold-ones) 5)) (list 1 1 1 1 1))

;; Ex 5.13 Use unfold to implement map, takeWhile, take, zipWith, and zipAll

(define unfold-stream-map :
  (All (A B) (-> (Stream A) (-> A B) (Stream B)))
  (λ (stream f)
    (unfold stream
            (λ ([state : (Stream A)]) : (Option (Pair B (Stream A)))
              (match state
                [(Sempty) (None)]
                [(Scons hd tl) (Some
                                (cons (f (force hd))
                                      (force tl)))])))))

(check-equal? (stream->list (unfold-stream-map (list->stream (list 1 2 3))
                                               add1))
              (list 2 3 4))


(define unfold-stream-takewhile :
  (All (A) (-> (Stream A) (-> A Boolean) (Stream A)))
  (λ (stream pred)
    (unfold stream
            (λ ([state : (Stream A)]) : (Option (Pair A (Stream A)))
              (match state
                [(Sempty) (None)]
                [(Scons hd tl)
                 (if (pred (force hd))
                     (Some (cons (force hd) (force tl)))
                     (None))])))))

(check-equal? (stream->list (unfold-stream-takewhile
                             (list->stream (list 2 4 6 7 8 9))
                             even?))
              (list 2 4 6))


(define unfold-stream-take-n :
  (All (A) (-> (Stream A) Integer (Stream A)))
  (λ (stream n)
    (unfold (cons stream n)
            (λ ([state : (Pair (Stream A) Integer)])
              (match state
                [(cons (Sempty) _) (None)]
                [(cons (Scons hd tl) n)
                 (if (= n 0)
                     (None)
                     (Some (cons (force hd) (cons (force tl) (sub1 n)))))])))))

(check-equal? (stream->list (unfold-stream-take-n
                             (list->stream (list 1 2 3 4 5)) 2))
              (list 1 2))

(define unfold-stream-zipwith :
  (All (A B C) (-> (Stream A) (Stream B) (-> A B C) (Stream C)))
  (λ (stream1 stream2 f)
    (unfold (cons stream1 (cons stream2 f))
            (λ ([state : (List* (Stream A) (Stream B) (-> A B C))]) :
              (Option (Pair C (List* (Stream A) (Stream B) (-> A B C))))
              (match state
                [(cons (Sempty) (cons _ _)) (None)]
                [(cons _ (cons (Sempty) _)) (None)]
                [(cons (Scons hd1 tl1) (cons (Scons hd2 tl2) f))
                 (Some (cons (f (force hd1) (force hd2))
                             (cons (force tl1) (cons (force tl2) f))))])))))

(check-equal? (stream->list (unfold-stream-zipwith
                             (list->stream (list 1 2 3))
                             (list->stream (list 4 5 6)) +))
              (list 5 7 9))

(check-equal? (stream->list (unfold-stream-zipwith
                             (list->stream '())
                             (list->stream (list 4 5 6)) +))
              '())
                                     
(check-equal? (stream->list (unfold-stream-zipwith
                             (list->stream (list 1 2 3))
                             (list->stream '()) +))
              '())

(check-equal? (stream->list (unfold-stream-zipwith
                             (list->stream (list 1 2))
                             (list->stream (list 4 5 6)) +))
              (list 5 7))


(define unfold-stream-zip :
  (All (A B) (-> (Stream A) (Stream B) (Stream (Pair A B))))
  (λ (stream1 stream2)
    (unfold (cons stream1 stream2)
            (λ ([state : (Pair (Stream A) (Stream B))]) :
              (Option (Pair (Pair A B) (Pair (Stream A) (Stream B))))
              (match state
                [(cons (Sempty) _) (None)]
                [(cons _ (Sempty)) (None)]
                [(cons (Scons hd1 tl1) (Scons hd2 tl2))
                 (Some (cons (cons (force hd1) (force hd2))
                             (cons (force tl1) (force tl2))))])))))

(check-equal? (stream->list (unfold-stream-zip (list->stream (list 1 2 3))
                                               (list->stream (list 4 5))))
              (list (cons 1 4)
                    (cons 2 5)))

(define unfold-stream-zipall :
  (All (A B) (-> (Stream A) (Stream B) (Stream (Pair (Option A) (Option B)))))
  (λ (stream1 stream2)
    (unfold (cons stream1 stream2)
            (λ ([state : (Pair (Stream A) (Stream B))]) :
              (Option (Pair (Pair (Option A) (Option B))
                            (Pair (Stream A) (Stream B))))
              (match state
                [(cons (Sempty) (Scons hd tl))
                 (Some (cons (cons (None) (Some (force hd)))
                             (cons (Sempty) (force tl))))]
                [(cons (Scons hd tl) (Sempty))
                 (Some (cons (cons (Some (force hd)) (None))
                             (cons (force tl) (Sempty))))]
                [(cons (Scons hd1 tl1) (Scons hd2 tl2))
                 (Some (cons (cons (Some (force hd1))
                                   (Some (force hd2)))
                             (cons (force tl1) (force tl2))))]
                [else (None)])))))

(check-equal? (stream->list (stream-map
                             (unfold-stream-zipall (list->stream (list 1 2 3))
                                                   (list->stream (list 4)))
                             (λ ([p : (Pair (Option Integer)
                                            (Option Integer))]) : Integer
                               (let* ([a (option-get-or-else (car p) 0)]
                                      [b (option-get-or-else (cdr p) 0)])
                                 (+ a b)))))
              (list 5 2 3))

;; Ex 5.14 Implement startsWith using functions you've written
(define starts-with? :
  (All (A B) (-> (Stream A) (Stream B) Boolean))
  (λ (stream1 stream2)
    (forall (unfold-stream-zipall stream1 stream2)
            (λ ([p : (Pair (Option A) (Option B))])
              (match p
                [(cons (None) (Some _)) #f]
                [(cons (Some _) (None)) #t]
                [(cons (Some v1) (Some v2)) (equal? v1 v2)]
                [else #t])))))

(check-equal? (starts-with? (list->stream (list 1 2 3))
                            (list->stream (list 1 2)))
              #t)
(check-equal? (starts-with? (list->stream (list 1 2))
                            (list->stream (list 1 2 3)))
              #f)
(check-equal? (starts-with? (list->stream (list 1 2 3))
                            (list->stream (list 1 2 3)))
              #t)
(check-equal? (starts-with? (list->stream (list 1 2))
                            (list->stream '()))
              #t)
(check-equal? (starts-with? (list->stream '())
                            (list->stream (list 1 2)))
              #f)

;; Ex 5.15 Implement tails using unfold. tails returns the stream of all
;; suffixes of a stream
(define tails :
  (All (A) (-> (Stream A) (Stream (Stream A))))
  (λ (stream)
    (unfold stream
            (λ ([state : (Stream A)])
              (match state
                [(Sempty) (None)]
                [(Scons _ tl) (Some (cons state (force tl)))])))))

;; We can now implement the hasSubsequence method from exercise 3.24 in an
;; optimal way without needing to explicitly manage a monolithic loop
(define has-subsequence? :
  (All (A) (-> (Stream A) (Stream A) Boolean))
  (λ (stream1 stream2)
    (exists2 (tails stream1)
             (λ ([s : (Stream A)])
               (starts-with? s stream2)))))

(check-equal? (has-subsequence? (list->stream (list 1 2 3 4))
                                (list->stream (list 1 2)))
              #t)
(check-equal? (has-subsequence? (list->stream (list 1 2 3 4))
                                (list->stream (list 2 3)))
              #t)
(check-equal? (has-subsequence? (list->stream (list 1 2 3 4))
                                (list->stream (list 4)))
              #t)
(check-equal? (has-subsequence? (list->stream (list 1 2 3 4))
                                (list->stream (list 4 5)))
              #f)

;; Ex 5.16 Generalize tails to the function scanRight, which is like a foldRight
;; that returns a stream of the intermediate results

;; NOTE: You cannot implement this using unfold (in linear time) since unfold
;; generates elements from left to right

(struct (A) ScanResult ([acc : A] [intermediate : (Listof A)]))

#;
;; Type checker doesn't infer B == (ScanResult B)...
(define scanright :
  (All (A B) (-> (-> A (Promise B) B)
                 (Promise (ScanResult B))
                 (Stream A)
                 (ScanResult B)))
  (λ (f z stream)
    (foldright (λ ([a : A] [b : (Promise (ScanResult B))]) :
                 (ScanResult B)
                 (let ([result (f a (delay (ScanResult-acc (force b))))])
                   (ScanResult result
                               (cons result
                                     (ScanResult-intermediate (force b))))))
               (delay (ScanResult (force z) empty))
               stream)))

;; The fix is to use `inst` to explicity instruct the type checker about which
;; types to use for the type parameters...why can't it infer it?
(define scanright :
  (All (A B) (-> (-> A (Promise B) B) (Promise B) (Stream A) (Listof B)))
  (λ (f z stream)
    (ScanResult-intermediate ((inst foldright A (ScanResult B))
                              (λ (a b)
                                (let ([result (f a (delay (ScanResult-acc
                                                           (force b))))])
                                  (ScanResult result
                                              (cons result
                                                    (ScanResult-intermediate
                                                     (force b))))))
                              (delay (ScanResult (force z) (list (force z))))
                              stream))))

(check-equal? (scanright (λ ([a : Integer] [b : (Promise Integer)])
                           (+ a (force b)))
                         (delay 0)
                         (list->stream (list 1 2 3)))
              (list 6 5 3 0))
