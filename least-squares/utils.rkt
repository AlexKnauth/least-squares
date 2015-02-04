#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/math
         racket/string
         math/matrix
         math/array
         math/number-theory
         syntax/parse/define
         (for-syntax racket/base syntax/parse unstable/syntax))
(module+ test (require rackunit))

(define-match-expander -:
  (syntax-parser [(-: pat:expr) #'(app - pat)])
  (make-variable-like-transformer #'-))

(define-match-expander hash-table:
  (syntax-parser [(hash-table: stuff ...) #'(hash-table stuff ...)])
  (syntax-parser [(hash-table: [k:expr v:expr] ...) #'(make-immutable-hash (list (cons k v) ...))]))

(define-simple-macro (define-function-struct name:id (field:id ...) [#:λ args result:expr ...+])
  (struct name (field ...) #:transparent
    #:property prop:procedure
    (let ([name (lambda (this . args)
                  (match-define (name field ...) this)
                  result ...)])
      name)))

(define-function-struct mx+b (m b) [#:λ (x) (+ (* m x) b)])
(define-function-struct ax^2+bx+c (a b c) [#:λ (x) (+ (* a (sqr x)) (* b x) c)])

(define-function-struct power-function (hsh)
  [#:λ (x) (for/sum ([(power coefficient) (in-hash hsh)])
             (* coefficient (expt x power)))])

(define-function-struct c*e^ax (c a) [#:λ (x) (* c (exp (* a x)))])

(define-function-struct multi-var-taylor-ish (lst)
  [#:λ args
       (define d (length args))
       (for/sum ([arr (in-list lst)]
                 [n (in-naturals)])
         (define arr.shape (array-shape arr))
         (unless (= n (vector-length arr.shape))
           (error 'multi-var-taylor-ish "wrong shape for array\n  shape: ~v\n  expected: ~v"
                  arr.shape (vector->immutable-vector (make-vector n d))))
         (for ([dim (in-vector arr.shape)])
           (unless (= dim d) (error 'multi-var-taylor-ish "\n  dim: ~v\n  d: ~v" dim d)))
         (define n! (factorial n))
         (* (/ n!)
            (for/sum ([p (in-array arr)]
                      [is (in-array-indexes arr.shape)])
              (* p (for/product ([i (in-vector is)])
                     (list-ref args i))))))])
(module+ test
  (define-syntax (chk stx)
    (define-syntax-class cls
      [pattern [a:expr e:expr] #:with norm (syntax/loc this-syntax (check-equal? a e))])
    (syntax-parse stx
      [(chk :cls ...) #'(begin norm ...)]))
  (test-case "multi-var-taylor-ish"
    (test-case "f() = 1"
      (define f
        (multi-var-taylor-ish
         (list (array 1))))
      (check-equal? (f) 1))
    (test-case "f(x) = 1 + 2*x"
      (define (g x) (+ 1 (* 2 x)))
      (define f
        (multi-var-taylor-ish
         (list (array 1)
               (array #[2]))))
      (check-equal? (f 0) 1)
      (check-equal? (f 1) 3)
      (check-equal? (f 2) 5)
      (for ([i (in-range 100)])
        (define x (exact-random/sgn 100))
        (check-equal? (f x) (g x))))
    (test-case "f(x) = 1 + 2*x + 3*x^2"
      (define (g x) (+ 1 (* 2 x) (* 3 (expt x 2))))
      (define f
        (multi-var-taylor-ish
         (list (array 1)
               (array #[2])
               (array #[#[6]]))))
      (check-equal? (f 0) 1)
      (check-equal? (f 1) 6)
      (check-equal? (f 2) 17)
      (for ([i (in-range 100)])
        (define x (exact-random/sgn 100))
        (check-equal? (f x) (g x))))
    (test-case "f(x,y) = 1 + 2*x + 3*y"
      (define (g x y) (+ 1 (* 2 x) (* 3 y)))
      (define f
        (multi-var-taylor-ish
         (list (array 1)
               (array #[2 3]))))
      (chk [(f 0 0) 1] [(f 1 0) 3] [(f 2 0) 5]
           [(f 0 1) 4] [(f 1 1) 6] [(f 2 1) 8]
           [(f 0 2) 7] [(f 1 2) 9] [(f 2 2) 11])
      (for ([i (in-range 100)])
        (define x (exact-random/sgn 100))
        (define y (exact-random/sgn 100))
        (check-equal? (f x y) (g x y))))
    (test-case "f(x,y) = 1 + 2*x + 3*y + 4*x^2 + 5*x*y + 6*y^2"
      (define (g x y) (+ 1 (* 2 x) (* 3 y) (* 4 (expt x 2)) (* 5 x y) (* 6 (expt y 2))))
      (define f
        (multi-var-taylor-ish
         (list (array 1)
               (array #[2 3])
               (array #[#[8 5] #[5 12]]))))
      (chk [(f 0 0)  1] [(f 1 0)  7] [(f 2 0) 21]
           [(f 0 1) 10] [(f 1 1) 21] [(f 2 1) 40]
           [(f 0 2) 31] [(f 1 2) 47] [(f 2 2) 71])
      (for ([i (in-range 100)])
        (define x (exact-random/sgn 100))
        (define y (exact-random/sgn 100))
        (check-equal? (f x y) (g x y))))
    ))

(define (ax+by+c a b c)
  (multi-var-taylor-ish
   (list (array c)
         (array #[a b]))))

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

(define-match-expander matrix:
  (syntax-parser [(matrix: [[pat:expr ...] ...])
                  #'(? matrix? (app matrix->list* (list (list pat ...) ...)))])
  (syntax-parser [(matrix: [[v:expr ...] ...])
                  #'(matrix [[v ...] ...])]))

(define-simple-macro (defmulti [id:id val:expr] ...)
  (begin (define id val) ...))

(define (function-struct->string f #:x [x "x"] #:y [y "y"])
  (define (d x)
    (cond [(integer? x) (inexact->exact x)]
          [else (exact->inexact x)]))
  (match f
    [(mx+b m b)        (format "~a = ~v*~a + ~v" y (d m) x (d b))]
    [(ax^2+bx+c a b c) (format "~a = ~v*~a^2 + ~v*~a + ~v" y (d a) x (d b) x (d c))]
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
            [1 (format "~v*~a" (d a) x)]
            [n (format "~v*~a^~v" (d a) x n)]))
        " + "))]
    [(c*e^ax c a) (format "~a = ~v*e^(~v*~a)" y (d c) (d a) x)]
    ))

(define (exact-random n)
  (+ (random n) (inexact->exact (random))))

(define (exact-random/sgn n)
  (define sgn (case (random 2) [(0) 1] [(1) -1]))
  (* sgn (exact-random n)))

