#lang racket/base

(provide (all-defined-out)
         (all-from-out "multi-var-taylor-ish.rkt"
                       "vector-function.rkt"))

(require racket/match
         syntax/parse/define
         "../utils.rkt"
         "multi-var-taylor-ish.rkt"
         "vector-function.rkt"
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


