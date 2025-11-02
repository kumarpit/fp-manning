#lang racket
(require "prelude.rkt")

;; Chapter 9: Parser Combinators

;;  Ex 9.1 Using `product`, implement `map2` and then use this to implement 
;;  many1 in terms of many.
;;
;;  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C]
;;
;;  Should be something like:
;;  a ** b map ((a, b) => f(a, b))
;;
;; NOTE: many is 0 or more, many1 is 1 or more
;;
;; def many1[A](p: Parser[A]) map2 p p.many (a, b) => a :: b

;; Ex 9.2 Try coming up with the laws to specify the behaviour of product
;;
;; Product is associative
;; a.map(f) ** b.map(f) == (a ** b).map((a, b) => (f(a), f(b))

;; Ex 9.3 Define `many` in terms of or, map2, and succeed
;; def many[A](p: Parser[A]): Parser[List[A]] 
;; 
;; Something like:
;; or(map2(p, p.many, (a, b) => a :: b), succeed(List()))
;;              ^ recursive combinator
;;              This means that the second argument to map2 should be lazy
;;              Otherwise, it will never terminate

;; Ex 9.4 Using map2 and succeed, implement the listOfN combinator from 
;; earlier
;; def listOfN[A](n: Int, p: Parser[A]) : Parser[List[A]]
;;
;; if n == 0 succeed(List())
;; else map2(p, listOfN(--n, p), (a, b) => (a :: b))

;; Ex 9.5 We could also deal with non-strictness with a separate combinator
;;
;; def defer[A](p: => Parser[A]) {
;;    input => p(input)
;; }
;;
;; Then using defer, we can many lazy like so:
;; def many[A](p: Parser[A]): Parser[List[A]] =
;;    map2(p, defer(many(p)))(_ :: _) or succeed(List())


;; Primitives so far in the parser combinator library:
;; - string(s)
;; - slice(p) -- returns the (consecutive) portion of input matched by p
;; - succeed(a)
;; - map(p)(f)
;; - product(p1, p2)
;; - or(p1, p2)

;; This is enough to create parsers for any context-free grammars!

;; Introducing a `flatMap` operator should be enough to allow for handling 
;; context-sensitivity
;; As example, suppose you want to parse a string like "4aaaa" such that you
;; parse number in the beginning and want to ensure there are exactly that many 
;; repititions in the following string. This is not possible using `product`
;; alone since the second parser in this case is dependent on the result of 
;; the first parser, and product provides no means to express this dependence.

;; def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

;; Ex 9.6 Using flatMap and other combinators, write the context-sensitive
;; parser we couldn't express earlier

;; Something like:
;; regex("[1-9][0-9]*".r).toInt.flatMap(n => listOfN(n, string("a"))

;; Ex 9.7 Implement `product` and `map2` in terms of `flatMap`
;; product => parserA.flatMap(resA => parserB.flatMap(resB => 
;; succeed(resA, resB))))
;; map2 => parserA.flatMap(resA => parserB.flatMap(resB => 
;; succeed(f(resA, resB))))
;; or equivalently:
;; map => parserA.flatMap(resA => parserB.map(resB => f(resA, resB))))

;; Ex 9.8 map is no longer primitive. Express it in terms of flatMap and/or 
;; other combinators
;; map = a.flatMap(res => succeed(f(res)))

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
