#lang typed/racket
(require "prelude.rkt")
(require "streams.rkt")

;; A lot of useful combinators Monad can be defined just in terms of map2 and
;; unit. While Monads provides unit and flatmap as primitives, if we decide to
;; pursue a similar abstraction with unit and map2 as primitives, we end up with
;; applicative functors -- these are less powerful than Monads. But these
;; limitations come with some benefits.

;; Again