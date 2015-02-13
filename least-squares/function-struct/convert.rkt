#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/string
         "multi-var-taylor-ish.rkt"
         "vector-function.rkt"
         "function-struct.rkt"
         "macros.rkt"
         "../utils.rkt"
         )

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
