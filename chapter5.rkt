#lang typed/racket
(require rackunit)
(require/typed
  rackunit
  [check-equal? (-> Any Any Void)])

;; Strictness and Laziness

;; Infinite computation, useful for testing
(: DIVERGE (-> Nothing))
(define (DIVERGE) (DIVERGE))

(struct Promise ([thunk : (-> Any)]))

;; Scheme-style promises (i.e in case of self-referential promise, returns the
;; *first* computed value - or diverge)
(: construct-promise (-> (-> Any) Promise))
(define (construct-promise thunk)
  (let* ([result-ready? : Boolean #f]
         [result : Any (void)])
    (Promise
     (λ () (let ([x (thunk)])
             (if result-ready?
                 result
                 (begin
                   (set! result-ready? #t)
                   (set! result x)
                   result)))))))

(: force (-> Promise Any))
(define (force p) ((Promise-thunk p)))
  
(define-syntax delay
  (syntax-rules ()
    [(_ expression)
     (construct-promise (λ () expression))]))

(check-equal? (Promise? (delay (DIVERGE))) #t)
(check-equal? (let ([p (delay (+ 9 7))])
        (force p))
      16)