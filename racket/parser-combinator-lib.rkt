#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Cursor (input offset))

;; Cursor Integer -> Cursor
;; advances the cursor by the given offset
(define (cursor/advance-by cursor i)
  (Cursor (Cursor-input cursor) (+ (Cursor-offset cursor) i)))

;; Cursor -> Integer
;; returns the current line number (1-indexed)
(define (cursor/line cursor)
  (for/fold ([acc 1]) ([ch (in-string (Cursor-input cursor)
                                      0
                                      (Cursor-offset cursor))])
    (if (char=? ch #\newline) (+ acc 1) acc)))

;; Cursor -> Integer
;; returns the current column number (1-indexed)
(define (cursor/col cursor)
  (let* ([input   (Cursor-input cursor)]
         [offset  (Cursor-offset cursor)]
         [matches (regexp-match-positions* #px"\n"
                                           input 0 offset)])
    (if (null? matches)
        (add1 offset)
        (- offset (car (last matches))))))

;; Cursor Integer -> String
;; return the slice of the input string starting at the current location to the
;; provided offset (relative to the current location)
(define (cursor/slice c i)
  (substring (Cursor-input c)
             (Cursor-offset c)
             (+ (Cursor-offset c) i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct ParseError (stack) #:transparent)
(struct ParseErrorFrame (message line col) #:transparent)

;; ParseError ParseErrorFrame -> ParseError
;; pushes a parse error frame onto the given parse error stack
(define (parse-error/push e frame)
  (ParseError (cons frame (ParseError-stack e))))

;; Cursor String -> ParseError
;; instantiates a ParseError from the given cursor state and error message
(define (cursor->error cursor str)
  (ParseError (list (ParseErrorFrame str
                                     (cursor/line cursor)
                                     (cursor/col cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Result[A] is one of
(struct Success (value consumed) #:transparent)  ; value : A, consumed : Integer
(struct Failure (parse-error committed?) #:transparent)

;; Result[A] (ParseError -> ParseError) -> Result[A]
;; transform the parse error given a failure
(define (result/map-error e f)
  (match e
    [(Failure e c) (Failure (f e) c)]
    [other other]))

;; Result[A] -> Result[A]
;; uncommits the failure from this result to enable backtracking
(define (result/uncommit res)
  (match res
    [(Failure e _) (Failure e #f)]
    [other other]))

;; Result[A] Integer -> Result[A]
;; if success, adds n to the consumed counter
(define (result/advance-success res n)
  (match res
    [(Success v m) (Success v (+ m n))]
    [other other]))

;; Result[A] Boolean -> Result[A]
;; if failure, commits the failure if c == true
(define (result/add-commit res c)
  (match res
    [(Failure e curr) (Failure e (or curr c))]
    [other other]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parser[A] is Cursor -> Result[A]

;; A -> Parser[A]
(define (succeed v) (λ (_) (Success v 0)))

;; Parser[Boolean]
(define eof
  (λ (c) (if (>= (Cursor-offset c) (string-length (Cursor-input c)))
             (Success #t 0)
             (Failure (cursor->error c "Expected eof") #f))))

;; String -> Parser[String]
;; matches on given string
(define (string str)
  ;; String String Integer -> (Maybe Integer)
  ;; returns the index of the first offset at which the strings don't match
  ;; #f if they match
  (define (first-mismatch-index input str offset)
    (let ([input-len (string-length input)])
      (for/first ([i (string-length str)]
                  #:when (not (and (< (+ offset i) input-len)
                                   (char=? (string-ref input (+ offset i))
                                           (string-ref str i)))))
        i)))
  
  (λ (c)
    (let ([i (first-mismatch-index (Cursor-input c) str
                                   (Cursor-offset c))])
      (if (not i)
          (Success str (string-length str))
          (Failure (cursor->error (cursor/advance-by c i) str)
                   (not (equal? i 0)))))))

;; Regexp -> Parser[String]
;; matches on given regex pattern at current cursor offset
(define (regex pat)
  (λ (c)
    (let* ([input  (Cursor-input c)]
           [offset (Cursor-offset c)]
           [m      (regexp-match-positions pat input offset)])
      (if (and m (= (caar m) offset))
          (let* ([end (cdar m)]
                 [str (substring input offset end)])
            (Success str (- end offset)))
          (Failure (cursor->error c (format "regex ~a" pat))
                   #f))))) ; regex matching is all-or-nothing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combinators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parser[A] (A -> Parser[B]) -> Parser[B]
;; allows context-sensitive parsing
(define (flatmap p1 f)
  (λ (c) (match (p1 c)
           [(Success value consumed)
            (let* ([eval-parser (f value)]
                   [nc          (cursor/advance-by c consumed)]
                   [res         (eval-parser nc)]
                   [res-with-commit (result/add-commit res (not (equal? consumed 0)))])
              (result/advance-success res-with-commit consumed))]
           [other other])))

;; Parser[A] (A -> B) -> Parser[B]
;; transforms the value returned by a parser
(define (p-map p f)
  (λ (c) (match (p c)
           [(Success v consumed) (Success (f v) consumed)]
           [other other])))

;; Parser[A] Parser[B] -> Parser[(List A B)]
;; sequences two parsers, returning both results as a list
(define (p-* p1 p2)
  (flatmap p1
           (λ (v1) (flatmap p2
                            (λ (v2) (succeed (list v1 v2)))))))

;; Parser[A] Parser[B] -> Parser[A]
;; sequences two parsers, returning the left result
(define (<* p1 p2)
  (flatmap p1
           (λ (v1) (flatmap p2
                            (λ (_) (succeed v1))))))

;; Parser[A] Parser[B] -> Parser[B]
;; sequences two parsers, returning the right result
(define (*> p1 p2)
  (flatmap p1
           (λ (_) (flatmap p2 succeed))))

;; Parser[A] Parser[A] -> Parser[A]
;; tries p1, and only if it fails in an uncommitted state, tries p2
(define (p-or p1 p2)
  (λ (c) (match (p1 c)
           [(Failure _ #f) (p2 c)]
           [other other])))

;; Parser[A] (-> Parser[B]) (A B -> C) -> Parser[C]
;; sequences two parsers lazily, combining results with f
;; lazy in p2 to support recursive grammars
(define (p-map2 p1 p2/th f)
  (p-map (flatmap p1
                  (λ (v1) (flatmap (p2/th)
                                   (λ (v2) (succeed (list v1 v2))))))
         (λ (vals) (apply f vals))))

;; Parser[A] -> Parser[Listof A]
;; matches p 0 or more times consecutively
(define (many p)
  (p-or (p-map2 p (λ () (many p)) cons)
        (succeed '())))

;; Parser[A] -> Parser[Listof A]
;; matches p 1 or more times consecutively
(define (many1 p)
  (p-map2 p (λ () (many p)) cons))

;; Parser[A] -> Parser[Listof A]
;; matches p N times
(define (listofN p N)
  (if (<= N 0)
      (succeed '())
      (p-map2 p (λ () (listofN p (- N 1))) cons)))

;; Parser[A] Parser[B] -> Parser[Listof A]
;; parses p one or more times separated by sep-p
(define (sep1 p sep-p)
  (flatmap p
           (λ (first)
             (p-map (many (*> sep-p p))
                    (λ (rest) (cons first rest))))))

;; Parser[A] Parser[B] -> Parser[Listof A]
;; parses p zero or more times separated by sep-p
(define (sep p sep-p)
  (p-or (sep1 p sep-p) (succeed '())))

;; Parser[A] -> Parser[A]
;; marks this parser as the root (i.e no other input is expected after this)
(define (root p) (<* p eof))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parser[A] -> Parser[A]
;; allows backtracking from this parser
(define (attempt p) (λ (c) (result/uncommit (p c))))

;; Parser[A] -> Parser[String]
;; returns the slice of the input string that was matched by the given parser
(define (slice p)
  (λ (c) (match (p c)
           [(Success _ consumed) (Success (cursor/slice c consumed) consumed)]
           [other other])))

;; String -> Parser[A] -> Parser[A]
;; wraps the given parser in a scope label for error reporting
(define (scope message)
  (λ (p)
    (λ (c)
      (result/map-error (p c)
                        (λ (e)
                          (parse-error/push e
                                            (ParseErrorFrame message
                                                             (cursor/line c)
                                                             (cursor/col c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parser[A] String -> Result[A]
(define (run p input) (p (Cursor input 0)))
