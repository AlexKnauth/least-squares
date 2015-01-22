#lang racket/base

(provide (all-defined-out))

(require racket/match
         math/matrix
         syntax/parse/define
         (for-syntax racket/base syntax/parse))

(struct mx+b (m b) #:transparent
  #:property prop:procedure
  (let ([mx+b (lambda (this x)
                (match-define (mx+b m b) this)
                (+ (* m x) b))])
    mx+b))

(define-match-expander matrix:
  (syntax-parser [(matrix: [[pat:expr ...] ...])
                  #'(? matrix? (app matrix->list* (list (list pat ...) ...)))]))

(define-simple-macro (defmulti [id:id val:expr] ...)
  (begin (define id val) ...))

