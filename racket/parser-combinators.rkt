#lang racket

;;  Ex 9.1 Using `product`, implement `map2` and then use this to implement 
;;  many1 in terms of many.

;; Product is the combinator to chain parsers (i.e run one after the other)
#| (define (map2 p1 p2 f)  |#
#|   (map (product p1 p2) f)) |#
#| (define (many1 p) (map2 p (many p) (lambda (a b) (cons a b)))) |#


;; Ex 9.2 Try coming up with the laws to specify the behaviour of product

;; Product is associative => (product a (product b c)) == (product (product a b) c)
;; ^ the resultant nesting is a little different but that can be resolved, conceptually they are the same
;; (product (map a f) (map b f)) == (map (product a b) (lambda (a, b) ((f a) (f b)))


;; Ex 9.3 Define `many` in terms of or, map2, and succeed

#| (define (many p) (or (map2 p (many p) cons) (succeed '()))) |#
;; insight here is that map2 is essentially the same as product in the sense it can be used to chain parsers
;; also, notice that the second argument to map2 must be lazy, otherwise this will never terminate


;; Ex 9.4 Using map2 and succeed, implement the listOfN combinator from 
;; earlier

#| (define (listofN p N) (if (zero? N)  |#
#|                         (succeed '()) |#
#|                         (map2 p (listofN p (- N 1)) cons))) |#


;; Ex 9.5 TODO: after chapter 7


;; Primitives so far in the parser combinator library:
;; - string(s)
;; - slice(p) -- returns the (consecutive) portion of input matched by p
;; - succeed(a)
;; - map(p)(f)
;; - product(p1, p2)
;; - or(p1, p2)

;; This is enough to create parsers for any context-free grammars!

;; Ex 9.6 Using flatMap and other combinators, write the context-sensitive
;; parser we couldn't express earlier

#| (define num-a (flatmap (regex "[1-9][0-9]*") (lambda (c) (listofN (char #\a) (char->integer c))))) |#

;; Ex 9.7 Implement `product` and `map2` in terms of `flatMap`

#| (define (product p1 p2) (flatmap p1 (lambda (a) (flatmap p2 (lambda (b) (succeed (a b))))))) |#
#| (define (map2 p1 p2 f) (flatmap p1 (lambda (a) (flatmap p2 (lambda (b) (succeed (f a b))))))) |#


;; Ex 9.8 map is no longer primitive. Express it in terms of flatMap and/or 
#| (define (map p f) (flatmap p (lambda (res) (succeed (f res))))) |#

;; We now have an even smaller set of primitives:
;; - string
;; - regex
;; - slice
;; - succeed
;; - or
;; - flatMap

;; Ex 9.9 Using these primitives, write a JSON parser!
#|

Parser[JSON]
(struct JNull () #:transparent)
(struct JNumber (num) #:transparent)
(struct JString (string) #:transparent)
(struct JBool (bool) #:transparent)
(struct JArray (array) #:transparent)
(struct JObject (hash) #:transparent)

whitespace = regex("\\s*")
token = string(s) <* whitespace -- <* product but ignore the RHS result
lit = token("null").as(JNull) |
      double.map(v => JNumber(v)) |
      quotedString.map(s => JString(s)) |
      token("true").as(JBool(true)) |
      token("false").as(JBool(false))

keyval = quotedString ** token(":") *> value
obj = token("{") *> keyval.sep(token(","))
                          .map(kvs => JKeyValue(kvs.toMap)) <* token("}")

array = token("[") *> value.sep(token(","))
                           .map(values => JArray(values)) <* token("]")

root = p <* eof
value = obj | lit | array
(whitespace *> (obj | array)).root
|#


