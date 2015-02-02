#lang sweet-exp racket/base

provide (all-defined-out)

require racket/match
        racket/math
        racket/string
        math/matrix
        syntax/parse/define
        for-syntax racket/base syntax/parse unstable/syntax

define-match-expander -:
  syntax-parser
    (-: pat:expr)
      #'(app - pat)
  make-variable-like-transformer #'-

define-match-expander hash-table:
  syntax-parser
    hash-table:(stuff ...)
      #'(hash-table stuff ...)
  syntax-parser
    hash-table:([k:expr v:expr] ...)
      #'(make-immutable-hash (list (cons k v) ...))

define-simple-macro
  define-function-struct name:id (field:id ...)
    #:λ args result:expr
  struct name (field ...) #:transparent #:property prop:procedure
    let ([name (lambda (this . args)
                  (match-define (name field ...) this)
                  result)])
      name

define ^ expt

define-function-struct mx+b (m b)
  #:λ (x) {{m * x} + b}
define-function-struct ax^2+bx+c (a b c)
  #:λ (x) {{a * (sqr x)} + {b * x} + c}

define-function-struct power-function (hsh)
  #:λ (x)
    for/sum ([(power coefficient) (in-hash hsh)])
      {coefficient * {x ^ power}}

define-function-struct c*e^ax (c a)
  #:λ (x) {c * exp{a * x}}

begin-for-syntax
  define-splicing-syntax-class x^n #:datum-literals (x^ ^ x) #:attributes (n)
    [pattern (~seq x ^ n:expr!x)]
    [pattern (~seq x^ n:expr!x)]
    [pattern (~seq id:id) #:do [(define str (symbol->string (syntax-e #'id)))]
             #:when (<= 3 (string-length str))
             #:when (string=? "x^" (substring str 0 2))
             #:do [(define n* (string->number (substring str 2)))]
             #:when n*
             #:with n (datum->syntax #'id n* #'id)]
    [pattern (~seq x) #:with n #'1]
  define-syntax-class expr!x
    [pattern (~and a:expr (~not (~parse [:x^n] #'[a])))]
  define-splicing-syntax-class power-function-seq
    [pattern
     (~seq (~or b:power-function-term b:+-power-function-term) c:+-power-function-term ...)
     #:with ([a:expr n:expr] ...) #'([b.a b.n] [c.a c.n] ...)]
  define-splicing-syntax-class power-function-term #:datum-literals (- x^ x) #:attributes (a n)
    [pattern (~seq a:expr!x :x^n)]
    [pattern (~seq a:expr!x) #:with n #'0]
    [pattern (~seq :x^n) #:with a #'1]
  define-syntax-class power-function-clause #:attributes (a n)
    [pattern [:power-function-term]]
  define-splicing-syntax-class +-power-function-term #:datum-literals (+ -)
    [pattern (~seq + b:power-function-term) #:with a #'b.a #:with n #'b.n]
    [pattern (~seq - -b:power-function-term) #:with a #'(-: -b.a) #:with n #'-b.n]


define-match-expander power-function:
  syntax-parser [(power-function: :power-function-clause ...)
                 #:with ooo (quote-syntax ...)
                 #'(power-function (hash-table [n a] ... [_ 0] ooo))]
                [(power-function: :power-function-seq)
                 #:with ooo (quote-syntax ...)
                 #'(power-function (hash-table [n a] ... [_ 0] ooo))]
  syntax-parser [(power-function: :power-function-clause ...)
                 #:with ooo (quote-syntax ...)
                 #'(power-function (hash-table: [n a] ...))]
                [(power-function: :power-function-seq)
                 #:with ooo (quote-syntax ...)
                 #'(power-function (hash-table: [n a] ...))]

define (mx+b->power-function f)
  match-define (mx+b m b) f
  power-function: m x + b

define (ax^2+bx+c->power-function f)
  match-define (ax^2+bx+c a b c) f
  power-function: a x^2 + b x + c

define (power-function->mx+b f)
  match-define (power-function: m x + b) f
  mx+b m b

define (power-function->ax^2+bx+c f)
  match-define (power-function: a x^2 + b x + c) f
  ax^2+bx+c a b c

define-match-expander matrix:
  syntax-parser
    (matrix: [[pat:expr ...] ...])
      #'(? matrix? (app matrix->list* (list (list pat ...) ...)))
  syntax-parser
    (matrix: [[v:expr ...] ...])
      #'(matrix [[v ...] ...])

define-simple-macro
  defmulti [id:id val:expr] ...
  begin (define id val) ...

define (function-struct->string f #:x [x "x"] #:y [y "y"])
  define (d x)
    cond [(integer? x) (inexact->exact x)]
         [else (exact->inexact x)]
  match f
    mx+b(m b)
      format("~a = ~v*~a + ~v" y (d m) x (d b))
    ax^2+bx+c(a b c)
      format("~a = ~v*~a^2 + ~v*~a + ~v" y (d a) x (d b) x (d c))
    power-function(hash-table())
      format("~a = 0" y)
    power-function(hsh)
      let* ([lst (hash->list hsh)]
            [lst (sort lst > #:key car)])
        string-join #:before-first format("~a = " y)
          for/list ([p (in-list lst)])
           match-define (cons n a) p
           match n
             0 (format "~v" (d a))
             1 (format "~v*~a" (d a) x)
             n (format "~v*~a^~v" (d a) x n)
          " + "
    c*e^ax(c a)
      format("~a = ~v*e^(~v*~a)" y (d c) (d a) x)


