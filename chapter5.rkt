#lang typed/racket
(require rackunit)
(require/typed
 rackunit
 [check-equal? (-> Any Any Void)])
(require/typed
 rackunit
 [check-exn (-> (-> Any Boolean) (-> Any) Void)])

(define-type (Option A) (U (Some A) (None A)))
(struct (A) None ())
(struct (A) Some ([value : A]))

(: option-get (All (A) (-> (Option A) A)))
(define (option-get opt)
  (match opt
    [(None) (error "Cannot get on an None option")]
    [(Some v) v]))

;; Strictness and Laziness

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

(define stream-filter :
  (All (A) (-> (Stream A) (-> A Boolean) (Stream A)))
  (λ (stream pred)
    (foldright (λ ([a : A] [b : (Promise (Stream A))]) : (Stream A)
                 (if (pred a)
                     (Scons (delay a) b)
                     (force b)))
               (delay (Sempty))
               stream)))

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