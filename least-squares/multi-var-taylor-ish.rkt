#lang racket/base

(provide multi-var-taylor-ish
         mx+b
         ax^2+bx+c
         ax+by+c
         ax+by+cz+d
         )

(require racket/match
         racket/splicing
         math/array
         math/number-theory
         "utils.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test (require rackunit))

(struct super-multi-var-taylor-ish (lst) #:transparent)

(define (make-multi-var-taylor-ish lst)
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
      (error 'multi-var-taylor-ish "wrong shape for array\n  shape: ~v\n  expected: ~v"
             arr.shape expected-shape)))
  (define constructor (get-multi-var-taylor-ish-constructor d))
  (constructor lst))

(splicing-local
    [;; sub-constructors : (HashTable Natural [(Listof (Arrayof Real)) -> Multi-Var-Taylor-Ish])
     (define sub-constructors (make-hash))
     ;; multi-var-taylor-ish : [Multi-Var-Taylor-Ish Real * -> Real]
     ;; will be passed as the procedure argument to prop:procedure
     (define (multi-var-taylor-ish f . args)
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
     ;; proc : [My-F Real * -> Real]
     (define proc multi-var-taylor-ish)]
  (define (get-multi-var-taylor-ish-constructor d)
    (hash-ref! sub-constructors d
      (lambda ()
        (struct multi-var-taylor-ish super-multi-var-taylor-ish () #:transparent
          #:property prop:procedure (procedure-reduce-arity proc (add1 d)))
        multi-var-taylor-ish))))

(define-match-expander multi-var-taylor-ish
  (syntax-parser
    [(multi-var-taylor-ish lst-pat:expr)
     #'(super-multi-var-taylor-ish lst-pat)])
  (make-var-like/pat #'make-multi-var-taylor-ish (~or :id (:id :expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (make-var-like/pat #'make-mx+b (~or :id (:id :expr :expr))))

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
  (make-var-like/pat #'make-ax^2+bx+c (~or :id (:id :expr :expr :expr))))


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
  (make-var-like/pat #'make-ax+by+c (~or :id (:id :expr :expr :expr))))

(define-match-expander ax+by+cz+d
  (syntax-parser [(ax+by+cz+d a:expr b:expr c:expr d:expr)
                  #'(multi-var-taylor-ish
                     (list (array: c)
                           (array: #[a b c])))])
  (make-var-like/pat #'make-ax+by+cz+d (~or :id (:id :expr :expr :expr :expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
