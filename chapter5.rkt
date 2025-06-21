#lang typed/racket
(require rackunit)
(require/typed
 rackunit
 [check-equal? (-> Any Any Void)])

(define-type (Option A) (U (Some A) (None A)))
(struct (A) None ())
(struct (A) Some ([value : A]))

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
(struct (A) Scons ([head : (Promise A)] [tail : (Stream A)]))

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
        [else (Scons (delay (first lst)) (list->stream (rest lst)))]))


;; Ex 5.1 Write a function to convert a stream to a list
(: stream->list (All (A) (-> (Stream A) (Listof A))))
(define (stream->list stream)
  (match stream
    [(Sempty) '()]
    [(Scons hd tl) (cons (force hd) (stream->list tl))]))
  
(check-equal? (Stream? (list->stream (list 1 2 3 4))) #t)
(check-equal? (list?
               (stream->list (Scons (delay 1) (Scons (delay 2) (Sempty))))) #t)

;; Ex 5.2 Implement take-n and drop-n for Stream
(: take-n (All (A) (-> (Stream A) Integer (Stream A))))
(define (take-n stream n)
  (cond [(<= n 0) (Sempty)]
        [else (match stream
                [(Sempty) (Sempty)]
                [(Scons hd tl) (Scons hd (take-n tl (sub1 n)))])]))

(: drop-n (All (A) (-> (Stream A) Integer (Stream A))))
(define (drop-n stream n)
  (match stream
    [(Sempty) (Sempty)]
    [(Scons hd tl) (if (<= n 0) stream (drop-n tl (sub1 n)))]))


;; Ex 5.3 Implement takeWhile for returning all starting elements in a stream
;; that match a given predicate
(: take-while (All (A) (-> (Stream A) (-> (Promise A) Boolean) (Stream A))))
(define (take-while stream pred)
  (match stream
    [(Sempty) (Sempty)]
    [(Scons hd tl) (if (pred hd)
                       (Scons hd (take-while tl pred))
                       (Sempty))]))


;; [5.3] Separating program description from evaluation

(: exists1 (All (A) (-> (Stream A) (-> A Boolean) Boolean)))
(define (exists1 stream pred)
  (match stream
    [(Sempty) #f]
    [(Scons hd tl) (if (pred (force hd)) #t (exists1 tl pred))]))

(: foldright (All (A B) (-> (-> A (Promise B) B) (Promise B) (Stream A) B)))
(define (foldright f z stream)
  (match stream
    [(Sempty) (force z)]
    ;; If f never evaluates its second argument, recursion never occurs -- this
    ;; way, we can achieve early termination!
    [(Scons hd tl) (f (force hd) (delay (foldright f z tl)))]))

;; Since foldright can now support early termination, we can use it to implement
;; exists1. This isn't possible with the original (eager) foldright. So
;; laziness has made our code more reusable. Note that this still isn't stack
;; safe!
(define exists2 :
  (All (A) (-> (Stream A) (-> A Boolean) Boolean))
  (λ (stream pred)
    (foldright (λ: ([a : A] [b : (Promise Boolean)]) : Boolean
                   (if (pred a) #t (force b)))
                 (delay #f)
                 stream)))

