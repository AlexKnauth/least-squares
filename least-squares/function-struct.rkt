#lang racket/base

(provide (all-defined-out)
         (all-from-out "multi-var-taylor-ish.rkt"))

(require racket/match
         racket/string
         syntax/parse/define
         "utils.rkt"
         "multi-var-taylor-ish.rkt"
         (for-syntax racket/base syntax/parse))

(define-simple-macro (define-function-struct name:id (field:id ...) [#:λ args result:expr ...+])
  (struct name (field ...) #:transparent
    #:property prop:procedure
    (let ([name (lambda (this . args)
                  (match-define (name field ...) this)
                  result ...)])
      name)))

(define-function-struct power-function (hsh)
  [#:λ (x) (for/sum ([(power coefficient) (in-hash hsh)])
             (* coefficient (expt x power)))])

(define-function-struct c*e^ax (c a) [#:λ (x) (* c (exp (* a x)))])


(begin-for-syntax
  (define-splicing-syntax-class x^n #:datum-literals (x^ ^ x)
    #:attributes (n)
    [pattern (~seq x ^ n:expr!x)]
    [pattern (~seq x^ n:expr!x)]
    [pattern (~seq id:id) #:do [(define str (symbol->string (syntax-e #'id)))]
             #:when (<= 3 (string-length str))
             #:when (string=? "x^" (substring str 0 2))
             #:do [(define n* (string->number (substring str 2)))]
             #:when n*
             #:with n (datum->syntax #'id n* #'id)]
    [pattern (~seq x) #:with n #'1])
  (define-syntax-class expr!x
    [pattern (~and a:expr (~not (~parse [:x^n] #'[a])))])
  (define-splicing-syntax-class power-function-seq
    [pattern
     (~seq (~or b:power-function-term b:+-power-function-term) c:+-power-function-term ...)
     #:with ([a:expr n:expr] ...) #'([b.a b.n] [c.a c.n] ...)])
  (define-splicing-syntax-class power-function-term #:datum-literals (- x^ x)
    #:attributes (a n)
    [pattern (~seq a:expr!x :x^n)]
    [pattern (~seq a:expr!x) #:with n #'0]
    [pattern (~seq :x^n) #:with a #'1])
  (define-syntax-class power-function-clause
    #:attributes (a n)
    [pattern [:power-function-term]])
  (define-splicing-syntax-class +-power-function-term #:datum-literals (+ -)
    [pattern (~seq + b:power-function-term) #:with a #'b.a #:with n #'b.n]
    [pattern (~seq - -b:power-function-term) #:with a #'(-: -b.a) #:with n #'-b.n])
  )

(define-match-expander power-function:
  (syntax-parser [(power-function: :power-function-clause ...)
                  #:with ooo (quote-syntax ...)
                  #'(power-function (hash-table [n a] ... [_ 0] ooo))]
                 [(power-function: :power-function-seq)
                  #:with ooo (quote-syntax ...)
                  #'(power-function (hash-table [n a] ... [_ 0] ooo))])
  (syntax-parser [(power-function: :power-function-clause ...)
                  #:with ooo (quote-syntax ...)
                  #'(power-function (hash-table: [n a] ...))]
                 [(power-function: :power-function-seq)
                  #:with ooo (quote-syntax ...)
                  #'(power-function (hash-table: [n a] ...))]))

(define (mx+b->power-function f)
  (match-define (mx+b m b) f)
  (power-function: m x + b))

(define (ax^2+bx+c->power-function f)
  (match-define (ax^2+bx+c a b c) f)
  (power-function: a x^2 + b x + c))

(define (power-function->mx+b f)
  (match-define (power-function: m x + b) f)
  (mx+b m b))

(define (power-function->ax^2+bx+c f)
  (match-define (power-function: a x^2 + b x + c) f)
  (ax^2+bx+c a b c))

(define (function-struct->string f #:y [y "y"] #:xs [xs '("x")])
  (define (d x)
    (cond [(integer? x) (inexact->exact x)]
          [else (exact->inexact x)]))
  (define (x i)
    (list-ref xs i))
  (match f
    [(mx+b m b)        (format "~a = ~v*~a + ~v" y (d m) (x 0) (d b))]
    [(ax^2+bx+c a b c) (format "~a = ~v*~a^2 + ~v*~a + ~v" y (d a) (x 0) (d b) (x 0) (d c))]
    [(power-function (hash-table)) (format "~a = 0" y)]
    [(power-function hsh)
     (let* ([lst (hash->list hsh)]
            [lst (sort lst > #:key car)])
       (string-join
        #:before-first (format "~a = " y)
        (for/list ([p (in-list lst)])
          (match-define (cons n a) p)
          (match n
            [0 (format "~v" (d a))]
            [1 (format "~v*~a" (d a) (x 0))]
            [n (format "~v*~a^~v" (d a) (x 0) n)]))
        " + "))]
    [(c*e^ax c a) (format "~a = ~v*e^(~v*~a)" y (d c) (d a) (x 0))]
    [(multi-var-taylor-ish (list)) (format "~a = 0" y)]
    [(multi-var-taylor-ish (list (array: a))) (format "~a = ~v" y a)]
    [(multi-var-taylor-ish (list (array: a) (array: #[b ...])))
     (string-join
      #:before-first (format "~a = ~v" y a)
      (for/list ([b (in-list b)] [x (in-list xs)])
        (format " + ~v*~a" b x))
      "")]
    ))

