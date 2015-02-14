#lang racket/base

(provide partial-derivative
         gradient
         )

(require racket/match
         racket/list
         racket/vector
         racket/math
         math/array
         syntax/parse/define
         "../utils.rkt"
         "multi-var-taylor-ish.rkt"
         "vector-function.rkt"
         "function-struct.rkt"
         "macros.rkt"
         (for-syntax racket/base
                     ))
(module+ test
  (require rackunit))

;; partial-derivative : Function-Struct Natural -> Function-Struct
(define (partial-derivative f i)
  (match f
    [(multi-var-taylor-ish lst)
     (multi-var-taylor-ish (partial-derivative/multi-var-taylor-ish-lst lst i))]
    [(vector-function lst)
     (vector-function (for/list ([f (in-list lst)])
                        (partial-derivative f i)))]
    [(power-function hsh)
     #:when (zero? i)
     (power-function (for/hash ([(power coefficient) (in-hash hsh)]
                                #:unless (or (zero? coefficient) (zero? power)))
                       (values (sub1 power) (* power coefficient))))]
    [(c*e^ax c a)
     #:when (zero? i)
     (c*e^ax (* a c) a)]
    ))

(define (partial-derivative/multi-var-taylor-ish-lst lst i)
  (match lst
    [(list) '()]
    [(list (array: C)) '()]
    ;[(list (array: C) (array: #[a ...]))
    ; (list (array: (list-ref/else a i (λ () 0))))]
    [(list-rest (array: C) rst)
     (match-define (vector d) (array-shape (first rst)))
     (for/list ([arr rst]
                [old-n (in-naturals 1)]
                [new-n (in-naturals)])
       (define new-shape (make-vector new-n d))
       (build-array new-shape
         (lambda (new-is)
           (/ (for/sum ([i2 (in-range old-n)])
                (define is (vector-insert new-is i2 i))
                (array-ref/else arr is (λ () 0)))
              old-n))))]
    ))

(define (vector-insert v i x)
  (define-values (v1 v2) (vector-split-at v i))
  (vector-append v1 (vector x) v2))

(define (gradient f)
  (define d (procedure-arity f))
  (vector-function
   (for/list ([i (in-range d)])
     (partial-derivative f i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define-simple-macro (defrandom id:id ...)
    (begin (define id (random)) ...))
  (defrandom C a b c d e)
  (define-check (check-f= f1 f2 ∆ n random-args)
    (for ([i (in-range n)])
      (define args (random-args))
      (check-= (apply f1 args) (apply f2 args) ∆)))
  (check-equal? (partial-derivative (multi-var-taylor-ish '()) 0) zero-f)
  (check-equal? (partial-derivative (const C) 0) zero-f)
  (check-equal? (partial-derivative (mx+b a C) 0) (const a))
  (check-equal? (partial-derivative (mx+b a C) 1) (const 0))
  (check-equal? (partial-derivative (ax^2+bx+c 3 5 7) 0) (mx+b 6 5))
  (check-equal? (partial-derivative (ax^2+bx+c a b C) 0) (mx+b (* 2 a) b))
  (check-equal? (partial-derivative (ax^2+bx+c a b C) 1) (mx+b 0 0))
  (check-equal? (partial-derivative (ax+by+c a b C) 0) (const a))
  (check-equal? (partial-derivative (ax+by+c a b C) 1) (const b))
  (check-equal? (partial-derivative (ax+by+c a b C) 2) (const 0))
  (test-case "f(x,y) = C + a*x + b*y + 1/2*c*x^2 + d*xy + 1/2*e*y^2"
    (define (g x y) (+ C (* a x) (* b y) (* 1/2 c (sqr x)) (* d x y) (* 1/2 e (sqr y))))
    (define (gx x y) (+ a (* c x) (* d y)))
    (define (gy x y) (+ b (* d x) (* e y)))
    (define f (multi-var-taylor-ish (list (array C) (array #[a b]) (array #[#[c d] #[d e]]))))
    (define fx (multi-var-taylor-ish (list (array a) (array #[c d]))))
    (define fy (multi-var-taylor-ish (list (array b) (array #[d e]))))
    (check-equal? (partial-derivative f 0) fx)
    (check-equal? (partial-derivative f 1) fy)
    (check-equal? (partial-derivative f 2) (multi-var-taylor-ish (list (array 0) (array #[0 0]))))
    (define (random-args) (list (random) (random)))
    (define ∆ 1e-15)
    (check-f= f g ∆ 100 random-args)
    (check-f= fx gx ∆ 100 random-args)
    (check-f= fy gy ∆ 100 random-args)
    )
  (check-equal? (partial-derivative
                 (power-function: a x^4 + b x^3 + c x^2 + d x + e) 0)
                (power-function: (* 4 a) x^3 + (* 3 b) x^2 + (* 2 c) x + d))
  (check-equal? (partial-derivative (c*e^ax c a) 0) (c*e^ax (* a c) a))
  (define-syntax mvt: (make-rename-transformer #'multi-var-taylor-ish:))
  (check-equal? (partial-derivative
                 (mvt: 7 x ^ 5 y + 13 x ^ 2 y ^ 3) 0)
                (mvt: (* 5 7) x ^ 4 y + (* 2 13) x y ^ 3))
  (check-equal? (partial-derivative
                 (mvt: 7 x ^ 5 y + 13 x ^ 2 y ^ 3) 1)
                (mvt: 7 x ^ 5 + (* 3 13) x ^ 2 y ^ 2))
  )

