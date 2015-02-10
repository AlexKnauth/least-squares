#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/math
         racket/string
         math/matrix
         math/array
         math/number-theory
         syntax/parse/define
         (for-syntax racket/base syntax/parse unstable/syntax syntax/name seq-no-order))
(module+ test (require rackunit))

(define-syntax app (make-rename-transformer #'#%app))

(define-simple-macro (defmatrix id:id [[e:expr ...] ...])
  (define id (matrix [[e ...] ...])))

(define-match-expander matrix:
  (syntax-parser [(matrix: [[pat:expr ...] ...])
                  #'(? matrix? (app matrix->list* (list (list pat ...) ...)))])
  (syntax-parser [(matrix: [[v:expr ...] ...])
                  #'(matrix [[v ...] ...])]))

(define-match-expander array:
  (lambda (stx)
    (define-syntax-class thing
      [pattern #[thng:thing ...] #:with lst* #'(list thng.lst* ...)]
      [pattern pat:expr #:with lst* #'pat])
    (syntax-parse stx
      [(array: thng:thing)
       #'(? array? (app array->list* thng.lst*))]))
  (syntax-parser [(array: thng)
                  #'(array thng)]))

(define-simple-macro (defmulti [id:id val:expr] ...)
  (begin (define id val) ...))

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

(define-function-struct power-function (hsh)
  [#:λ (x) (for/sum ([(power coefficient) (in-hash hsh)])
             (* coefficient (expt x power)))])

(define-function-struct c*e^ax (c a) [#:λ (x) (* c (exp (* a x)))])

(define (app-multi-var-taylor-ish f . args)
  (match-define (super-multi-var-taylor-ish lst) f)
  (define d (length args))
  (for/sum ([arr (in-list lst)]
            [n (in-naturals)])
    (define arr.shape (array-shape arr))
    (define n! (factorial n))
    (* (/ n!)
       (for/sum ([p (in-array arr)]
                 [is (in-array-indexes arr.shape)])
         (* p (for/product ([i (in-vector is)])
                (list-ref args i)))))))

(struct super-multi-var-taylor-ish (lst) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b sub-equal?)
     (match-define (super-multi-var-taylor-ish a.lst) a)
     (match-define (super-multi-var-taylor-ish b.lst) b)
     (sub-equal? a.lst b.lst))
   (define (hash-proc a sub-hash-code)
     (match-define (super-multi-var-taylor-ish a.lst) a)
     (add1 (sub-hash-code a.lst)))
   (define (hash2-proc a sub-hash-code)
     (match-define (super-multi-var-taylor-ish a.lst) a)
     (+ 2 (sub-hash-code a.lst)))])

(define (make-multi-var-taylor-ish lst #:name [name 'multi-var-taylor-ish])
  (define d
    (match lst
      [(list) 0]
      [(list _) 0]
      [(list _ (array: #[a ...]) _ ...) (length a)]))
  (for ([arr (in-list lst)]
        [n   (in-naturals)])
    (define arr.shape (array-shape arr))
    (define expected-shape (vector->immutable-vector (make-vector n d)))
    (unless (equal? arr.shape expected-shape)
      (error name "wrong shape for array\n  shape: ~v\n  expected: ~v"
             arr.shape expected-shape)))
  (struct multi-var-taylor-ish super-multi-var-taylor-ish () #:transparent
    #:property prop:procedure
    (procedure-reduce-arity
     (procedure-rename app-multi-var-taylor-ish name)
     (add1 d)))
  (multi-var-taylor-ish lst))

(define-match-expander multi-var-taylor-ish
  (syntax-parser
    [(multi-var-taylor-ish lst-pat:expr)
     #'(super-multi-var-taylor-ish lst-pat)])
  (lambda (stx)
    (syntax-parse stx
      [(multi-var-taylor-ish lst:expr)
       #:with name:id (syntax-local-infer-name stx)
       #'(make-multi-var-taylor-ish lst #:name 'name)]
      [(multi-var-taylor-ish lst:expr)
       #'(make-multi-var-taylor-ish lst)]
      [(multi-var-taylor-ish . (~and stuff (~no-order lst:expr (~seq #:name name:expr))))
       #'(make-multi-var-taylor-ish . stuff)]
      [make-multi-var-taylor-ish:id #'make-multi-var-taylor-ish])))

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
      (check-equal? (procedure-arity f) 0)
      (check-equal? (f) 1))
    (test-case "f(x) = 1 + 2*x"
      (define (g x) (+ 1 (* 2 x)))
      (define f
        (multi-var-taylor-ish
         (list (array 1)
               (array #[2]))))
      (check-equal? (procedure-arity f) 1)
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
      (check-equal? (procedure-arity f) 1)
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
      (check-equal? (procedure-arity f) 2)
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
      (check-equal? (procedure-arity f) 2)
      (chk [(f 0 0)  1] [(f 1 0)  7] [(f 2 0) 21]
           [(f 0 1) 10] [(f 1 1) 21] [(f 2 1) 40]
           [(f 0 2) 31] [(f 1 2) 47] [(f 2 2) 71])
      (for ([i (in-range 100)])
        (define x (exact-random/sgn 100))
        (define y (exact-random/sgn 100))
        (check-equal? (f x y) (g x y))))
    ))

(define (make-mx+b m b)
  (multi-var-taylor-ish
   (list (array b)
         (array #[m]))))

(define-match-expander mx+b
  (syntax-parser
    [(mx+b m:expr b:expr)
     #'(multi-var-taylor-ish
        (list (array: b)
              (array: #[m])))])
  (make-variable-like-transformer #'make-mx+b))

(define (make-ax^2+bx+c a b c)
  (multi-var-taylor-ish
   (list (array c)
         (array #[b])
         (array #[#[(* 2 c)]]))))

(define-match-expander ax^2+bx+c
  (syntax-parser
    [(ax^2+bx+c a:expr b:expr c:expr)
     #'(multi-var-taylor-ish
        (list (array: c)
              (array: #[b])
              (array: #[#[(app (λ (x) (* 1/2 x)) a)]])))])
  (make-variable-like-transformer #'make-ax^2+bx+c))


(define (make-ax+by+c a b c)
  (multi-var-taylor-ish
   (list (array c)
         (array #[a b]))))

(define (make-ax+by+cz+d a b c d)
  (multi-var-taylor-ish
   (list (array d)
         (array #[a b c]))))

(define-match-expander ax+by+c
  (syntax-parser [(ax+by+c a:expr b:expr c:expr)
                  #'(multi-var-taylor-ish
                     (list (array: c)
                           (array: #[a b])))])
  (make-variable-like-transformer #'make-ax+by+c))

(define-match-expander ax+by+cz+d
  (syntax-parser [(ax+by+cz+d a:expr b:expr c:expr d:expr)
                  #'(multi-var-taylor-ish
                     (list (array: c)
                           (array: #[a b c])))])
  (make-variable-like-transformer #'make-ax+by+cz+d))

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

(define (exact-random n)
  (+ (random n) (inexact->exact (random))))

(define (exact-random/sgn n)
  (define sgn (case (random 2) [(0) 1] [(1) -1]))
  (* sgn (exact-random n)))

