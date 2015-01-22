#lang racket/base

(provide linear-least-squares
         (struct-out mx+b))

(require racket/match
         racket/math
         math/matrix
         syntax/parse/define
         "utils.rkt"
         (for-syntax racket/base syntax/parse))
(module+ test (require rackunit))

(define (linear-least-squares points)
  (define-simple-macro (∑ expr:expr)
    #:with [x-id y-id] (syntax-local-introduce #'[xi yi])
    (for/sum ([p (in-list points)])
      (match-define (list x-id y-id) p)
      expr))
  ;; D = ∑[(yi - [a*xi + b])^2]
  ;; ∂D/∂a = ∑[2*(yi - [a*xi + b])*(-xi)] = ∑[2*(xi^2*a + xi*b - xi*yi)]
  ;; ∂D/∂b = ∑[2*(yi - [a*xi + b])*(-1)]  = ∑[2*(xi*a + b - yi)]
  ;; 0 = ∑[xi^2*a + xi*b - yi] = ∑[xi^2]*a + ∑[xi]*b - ∑[xi*yi] = 0
  ;; 0 = ∑[xi*a + b - yi]      = ∑[xi]*a + n*b - ∑[yi]          = 0
  ;; ∑[xi^2]*a + ∑[xi]*b = ∑[xi*yi]
  ;;   ∑[xi]*a +     n*b = ∑[yi]
  (defmulti
    [n (length points)]
    [∑xi^2  (∑ (sqr xi))]
    [∑xi    (∑ xi)]
    [∑xi*yi (∑ (* xi yi))]
    [∑yi    (∑ yi)])
  ;; [[ ∑xi^2  ∑xi ]  . [[ a ]   = [[ ∑xi*yi ]
  ;;  [  ∑xi    n  ]]    [ b ]]     [  ∑yi   ]]
  (define M (matrix [[ ∑xi^2  ∑xi ]
                     [  ∑xi    n  ]]))
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (matrix [[ ∑xi*yi ]
                                   [  ∑yi   ]])))
  (match-define (matrix: [[a] [b]]) X)
  (mx+b a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (linear-least-squares '([0 0] [1 1] [2 2])) (mx+b 1 0))
  (check-equal? (linear-least-squares '([0 0] [1 2] [2 1])) (mx+b 1/2 1/2))
  )
