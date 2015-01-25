#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/math
         math/matrix
         syntax/parse/define
         (for-syntax racket/base syntax/parse))

(define-simple-macro (define-function-struct name:id (field:id ...) [#:λ args result:expr])
  (struct name (field ...) #:transparent
    #:property prop:procedure
    (let ([name (lambda (this . args)
                  (match-define (name field ...) this)
                  result)])
      name)))

(define-function-struct mx+b (m b) [#:λ (x) (+ (* m x) b)])
(define-function-struct ax^2+bx+c (a b c) [#:λ (x) (+ (* a (sqr x)) (* b x) c)])

(define-function-struct power-function (hsh)
  [#:λ (x) (for/sum ([(power coefficient) (in-hash hsh)])
             (* coefficient (expt x power)))])

(begin-for-syntax
  (define-syntax-class power-function-clause #:datum-literals (x^ x)
    [pattern [a:expr x^ n:expr]]
    [pattern [a:expr x] #:with n #'1]
    [pattern [a:expr] #:with n #'0]))

(define-match-expander power-function:
  (syntax-parser [(power-function: :power-function-clause ...)
                  #:with ooo (quote-syntax ...)
                  #'(power-function (hash-table [n a] ... [_ 0] ooo))]))

(define-match-expander matrix:
  (syntax-parser [(matrix: [[pat:expr ...] ...])
                  #'(? matrix? (app matrix->list* (list (list pat ...) ...)))]))

(define-simple-macro (defmulti [id:id val:expr] ...)
  (begin (define id val) ...))

