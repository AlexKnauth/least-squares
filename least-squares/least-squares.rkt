#lang racket/base

(provide linear-least-squares
         quadratic-least-squares
         polynomial-least-squares
         best-polynomial
         exponential-least-squares/logy
         (struct-out mx+b)
         (struct-out ax^2+bx+c)
         (struct-out power-function)
         (struct-out c*e^ax)
         power-function:
         )

(require racket/match
         racket/list
         math/matrix
         syntax/parse/define
         infix/infix-macro
         "utils.rkt"
         (for-syntax racket/base syntax/parse))
(module+ test (require rackunit))

(define (linear-least-squares points)
  (define-simple-macro (∑ expr:expr ...)
    #:with [x-id y-id] (syntax-local-introduce #'[xi yi])
    (for/sum ([p (in-list points)])
      (match-define (list x-id y-id) p)
      (: expr ...)))
  ;; D = ∑[([a*xi + b] - yi)^2]
  ;; ∂D/∂a = ∑[2*([a*xi + b] - yi)*xi] = ∑[2*(xi^2*a + xi*b - xi*yi)]
  ;; ∂D/∂b = ∑[2*([a*xi + b] - yi)*1]  = ∑[2*(xi*a + b - yi)]
  ;; 0 = ∑[xi^2*a + xi*b - yi] = ∑[xi^2]*a + ∑[xi]*b - ∑[xi*yi] = 0
  ;; 0 = ∑[xi*a + b - yi]      = ∑[xi]*a + n*b - ∑[yi]          = 0
  ;; ∑[xi^2]*a + ∑[xi]*b = ∑[xi*yi]
  ;;   ∑[xi]*a +     n*b = ∑[yi]
  (defmulti
    [n (length points)]
    [∑xi^2  (∑ xi ^ 2)]
    [∑xi    (∑ xi)]
    [∑xi*yi (∑ xi * yi)]
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

(define (quadratic-least-squares points)
  (define-simple-macro (∑ expr:expr ...)
    #:with [x-id y-id] (syntax-local-introduce #'[xi yi])
    (for/sum ([p (in-list points)])
      (match-define (list x-id y-id) p)
      (: expr ...)))
  ;; D = ∑[([a*xi^2 + b*xi + c] - yi)^2]
  ;; ∂D/∂a = ∑[2*([a*xi^2 + b*xi + c] - yi)*xi^2] = ∑[2*(xi^4*a + xi^3*b + xi^2*c - xi^2*yi)]
  ;; ∂D/∂b = ∑[2*([a*xi^2 + b*xi + c] - yi)*xi]   = ∑[2*(xi^3*a + xi^2*b + xi*c - xi*yi)]
  ;; ∂D/∂c = ∑[2*([a*xi^2 + b*xi + c] - yi)*1]    = ∑[2*(xi^2*a + xi*b + c - yi)]
  ;; ∑[xi^4]*a + ∑[xi^3]*b + ∑[xi^2]*c = ∑[xi^2*yi]
  ;; ∑[xi^3]*a + ∑[xi^2]*b +   ∑[xi]*c = ∑[xi*yi]
  ;; ∑[xi^2]*a +   ∑[xi]*b +       n*c = ∑[yi]
  (defmulti
    [n (length points)]
    [∑xi^4    (∑ xi ^ 4)]
    [∑xi^3    (∑ xi ^ 3)]
    [∑xi^2    (∑ xi ^ 2)]
    [∑xi      (∑ xi)]
    [∑xi^2*yi (∑ xi ^ 2 * yi)]
    [∑xi*yi   (∑ xi * yi)]
    [∑yi      (∑ yi)])
  ;; [[ ∑xi^4  ∑xi^3  ∑xi^2 ]    [[ a ]    [[ ∑xi^2*yi ]
  ;;  [ ∑xi^3  ∑xi^2   ∑xi  ]  *  [ b ]  =  [  ∑xi*yi  ]
  ;;  [ ∑xi^2   ∑xi     n   ]]    [ c ]]    [   ∑yi    ])
  (define M (matrix [[ ∑xi^4  ∑xi^3  ∑xi^2 ]
                     [ ∑xi^3  ∑xi^2   ∑xi  ]
                     [ ∑xi^2   ∑xi     n   ]]))
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (matrix [[ ∑xi^2*yi ]
                                   [  ∑xi*yi  ]
                                   [   ∑yi    ]])))
  (match-define (matrix: [[a] [b] [c]]) X)
  (ax^2+bx+c a b c))

(define (polynomial-least-squares n points)
  (define-simple-macro (∑ expr:expr ...)
    #:with [x-id y-id] (syntax-local-introduce #'[xi yi])
    (for/sum ([p (in-list points)])
      (match-define (list x-id y-id) p)
      (: expr ...)))
  (define |(list ∑xi^(2n) ∑xi^(2n-1) ... ∑xi^0)|
    (for/list ([k (in-range (: 2 * n) -1 -1)])
      (∑ xi ^ k)))
  (define |(list ∑xi^n*yi ∑xi^(n-1)*yi ... ∑xi^0*yi)|
    (for/list ([k (in-range n -1 -1)])
      (∑ xi ^ k * yi)))
  (define M-list* (for/list ([row-num (in-range (add1 n))])
                    (for/list ([col-num (in-range (add1 n))])
                      (list-ref |(list ∑xi^(2n) ∑xi^(2n-1) ... ∑xi^0)|
                                (+ row-num col-num)))))
  (define M (list*->matrix M-list*))
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (->col-matrix
                           (for/list ([row-num (in-range (add1 n))])
                             (list-ref |(list ∑xi^n*yi ∑xi^(n-1)*yi ... ∑xi^0*yi)| row-num)))))
  (define X-list (matrix->list X))
  (power-function
   (for/hash ([k (in-range n -1 -1)]
              [v (in-list X-list)])
     (values k v))))

(define (best-polynomial points)
  (polynomial-least-squares (sub1 (length points)) points))

(define (exponential-least-squares/logy points)
  (define points/logy
    (for/list ([p (in-list points)])
      (match-define (list x y) p)
      (list x (log y))))
  (match-define (mx+b m b) (linear-least-squares points/logy))
  ;; e^(mx+b) = e^b*e^mx
  (c*e^ax (exp b) m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (linear-least-squares '([0 0] [1 1] [2 2])) (mx+b 1 0))
  (check-equal? (linear-least-squares '([0 0] [1 2] [2 1])) (mx+b 1/2 1/2))
  (check-equal? (linear-least-squares '([0 1] [2 1] [3 4])) (mx+b 6/7 4/7))
  (check-equal? (quadratic-least-squares '([0 0] [1 1] [2 4])) (ax^2+bx+c 1 0 0))
  (check-equal? (quadratic-least-squares '([0 1] [-1 0] [2 -3])) (ax^2+bx+c -1 0 1))
  (check-equal? (quadratic-least-squares '([0 1] [2 1] [3 4])) (ax^2+bx+c 1 -2 1))
  (check-match (polynomial-least-squares 1 '([0 0] [1 1] [2 2])) (power-function: x))
  (check-match (polynomial-least-squares 1 '([0 0] [1 2] [2 1])) (power-function: 1/2 x + 1/2))
  (check-match (polynomial-least-squares 2 '([0 0] [1 1] [2 4])) (power-function: 1 x^2))
  (check-match (polynomial-least-squares 2 '([0 1] [-1 0] [2 -3])) (power-function: - x^2 + 1))
  (define (exact-random n)
    (* (case (random 2) [(0) 1] [(1) -1])
       (+ (random n) (inexact->exact (random)))))
  (test-case "best-polynomial"
    (for ([n (in-range 1 15)])
      (define ps
        (for/list ([i (in-range n)])
          (list (exact-random 100) (exact-random 100))))
      (define f (best-polynomial ps))
      (for ([p (in-list ps)])
        (match-define (list x y) p)
        (check-equal? (f x) y))))
  (check-equal? (exponential-least-squares/logy '([-2 1/4] [-1 1/2] [0 1.0] [1 2] [2 4]))
                (c*e^ax 1.0 (log 2)))
  )
