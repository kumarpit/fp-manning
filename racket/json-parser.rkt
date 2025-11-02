#lang racket

(require "parser-combinator-lib.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON Data Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct JNull   ()      #:transparent)
(struct JBool   (bool)  #:transparent)
(struct JNumber (num)   #:transparent)
(struct JString (str)   #:transparent)
(struct JArray  (items) #:transparent)
(struct JObject (pairs) #:transparent)  ; pairs : Immutable Hash String -> JSON

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ws (regex #px"\\s*"))

;; Parser[A] -> Parser[A]  (skips trailing whitespace)
(define (token p) (<* p ws))

;; String -> Parser[String]
(define (lit s) (token (string s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Value Parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define null-p
  ((scope "null")
   (p-map (lit "null") (λ (_) (JNull)))))

(define bool-p
  ((scope "bool")
   (p-or (p-map (lit "true")  (λ (_) (JBool #t)))
         (p-map (lit "false") (λ (_) (JBool #f))))))

(define number-p
  ((scope "number")
   (p-map (token (slice (regex #px"-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?")))
          (λ (s) (JNumber (string->number s))))))

;; Parser[String] — raw string content with quotes stripped
(define quoted-string-p
  ((scope "string")
   (p-map (token (slice (regex #px"\"(?:[^\"\\\\]|\\\\.)*\"")))
          (λ (s) (substring s 1 (- (string-length s) 1))))))

(define string-p (p-map quoted-string-p JString))

(define lit-p
  ((scope "literal")
   (p-or null-p
         (p-or bool-p
               (p-or number-p string-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive Parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; forward declaration — body is evaluated lazily at call time,
;; by which point array-p and obj-p are both defined
(define (value-p c)
  (((scope "value") (p-or lit-p (p-or array-p obj-p))) c))

(define array-p
  ((scope "array")
   (*> (lit "[")
       (<* (p-map (sep value-p (lit ",")) JArray)
           (lit "]")))))

;; Parser[(Pair String JSON)]
(define keyval-p
  ((scope "key-value")
   (p-map2 quoted-string-p
           (λ () (*> (lit ":") value-p))
           cons)))

(define obj-p
  ((scope "object")
   (*> (lit "{")
       (<* (p-map (sep keyval-p (lit ","))
                  (λ (kvs) (JObject (make-immutable-hash kvs))))
           (lit "}")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parser[JSON] — skips leading whitespace, accepts any JSON value
(define json-p (root (*> ws (p-or obj-p (p-or array-p lit-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  ;; primitives
  (check-equal? (Success-value (run json-p "null"))      (JNull))
  (check-equal? (Success-value (run json-p "true"))      (JBool #t))
  (check-equal? (Success-value (run json-p "false"))     (JBool #f))
  (check-equal? (Success-value (run json-p "42"))        (JNumber 42))
  (check-equal? (Success-value (run json-p "3.14"))      (JNumber 3.14))
  (check-equal? (Success-value (run json-p "-1e10"))     (JNumber -1e10))
  (check-equal? (Success-value (run json-p "\"hello\"")) (JString "hello"))

  ;; arrays
  (check-equal? (Success-value (run json-p "[]"))
                (JArray '()))
  (check-equal? (Success-value (run json-p "[1, 2, 3]"))
                (JArray (list (JNumber 1) (JNumber 2) (JNumber 3))))
  (check-equal? (Success-value (run json-p "[true, null, \"hi\"]"))
                (JArray (list (JBool #t) (JNull) (JString "hi"))))

  ;; objects
  (check-equal? (Success-value (run json-p "{}"))
                (JObject (make-immutable-hash '())))
  (check-equal? (Success-value (run json-p "{\"a\": 1, \"b\": 2}"))
                (JObject (make-immutable-hash (list (cons "a" (JNumber 1))
                                                    (cons "b" (JNumber 2))))))

  ;; nested
  (check-equal? (Success-value (run json-p "{\"xs\": [1, 2], \"nested\": {\"ok\": true}}"))
                (JObject (make-immutable-hash
                          (list (cons "xs"     (JArray (list (JNumber 1) (JNumber 2))))
                                (cons "nested" (JObject (make-immutable-hash
                                                         (list (cons "ok" (JBool #t))))))))))

  ;; leading whitespace
  (check-equal? (Success-value (run json-p "  42"))    (JNumber 42))
  (check-equal? (Success-value (run json-p "\n null")) (JNull))

  ;; failures
  (check-pred Failure? (run json-p "{"))
  (check-pred Failure? (run json-p "[1, 2,]"))
  (check-pred Failure? (run json-p "")))
