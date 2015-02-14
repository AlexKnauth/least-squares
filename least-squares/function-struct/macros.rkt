#lang racket/base

(provide (all-defined-out))

(require racket/match
         "multi-var-taylor-ish.rkt"
         "vector-function.rkt"
         "function-struct.rkt"
         "../utils.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/match
                     racket/list
                     math/base
                     (only-in math/number-theory factorial)
                     math/array
                     unstable/list
                     ))
(module+ test
  (require rackunit))

(begin-for-syntax
  (define here #'())
  (define-syntax-class id->str
    #:attributes (sym str length)
    [pattern id:id
             #:attr sym (syntax-e #'id)
             #:attr str (symbol->string (attribute sym))
             #:attr length (string-length (attribute str))])
  (define-syntax-class xi
    #:attributes (i)
    [pattern (~datum x) #:attr i 0]
    [pattern (~datum y) #:attr i 1]
    [pattern (~datum z) #:attr i 2]
    [pattern (~datum u) #:attr i 2]
    [pattern (~datum v) #:attr i 3]
    [pattern xi:id->str #:do [(define str (attribute xi.str))]
             #:when (<= 2 (attribute xi.length))
             #:when (string=? "x" (substring str 0 1))
             #:do [(define i* (string->number (substring str 1)))]
             #:when i*
             #:attr i i*])
  (define-splicing-syntax-class xi^n #:datum-literals (^)
    #:attributes (i n)
    [pattern (~seq :xi ^ nstx:number) #:attr n (syntax->datum #'nstx)]
    [pattern (~seq id:id->str nstx:number) #:do [(define len (attribute id.length))
                                                 (define str (attribute id.str))]
             #:attr n (syntax->datum #'nstx)
             #:when (<= 3 len)
             #:when (string=? "^" (substring str (- len 2) (- len 1)))
             #:with :xi (string->symbol (substring str 0 (- len 2)))]
    [pattern (~seq id:id->str) #:when (regexp-match? #rx"^([^^]+)^([^^]+)$" (attribute id.str))
             #:do [(match-define (regexp #rx"^([^^]+)^([^^]+)$" (list _ xi-str n-str))
                     (attribute id.str))]
             #:with :xi (string->symbol xi-str)
             #:do [(define n* (string->number n-str))]
             #:when n*
             #:attr n n*]
    [pattern (~seq :xi) #:attr n 1])
  (define-splicing-syntax-class x^n #:datum-literals (x^ ^ x)
    #:attributes (n)
    [pattern (~seq x ^ n:expr!x)]
    [pattern (~seq x^ n:expr!x)]
    [pattern (~seq id:id->str) #:do [(define str (attribute id.str))]
             #:when (<= 3 (attribute id.length))
             #:when (string=? "x^" (substring str 0 2))
             #:do [(define n* (string->number (substring str 2)))]
             #:when n*
             #:with n (datum->syntax #'id n* #'id)]
    [pattern (~seq x) #:with n #'1])
  (define-syntax-class expr!x
    [pattern (~and a:expr (~not (~parse [(~or :x^n :xi^n)] #'[a])))])
  (define-splicing-syntax-class multi-var-taylor-ish-seq
    [pattern
     (~seq (~or b:multi-var-taylor-ish-term b:+-multi-var-taylor-ish-term)
           c:+-multi-var-taylor-ish-term ...)
     #:do [(define ns (cons (attribute b.n) (attribute c.n)))
           (define ds (cons (attribute b.d) (attribute c.d)))
           (define shapes (cons (attribute b.shape) (attribute c.shape)))
           (define iss (cons (attribute b.is) (attribute c.is)))
           (define as (cons (attribute b.a) (attribute c.a)))
           (define max-n (apply max ns))
           (define d (apply max ds))
           (define stuff
             (for/fold ([hsh (hash)]) ([n (in-list ns)] [is (in-list iss)] [a (in-list as)])
               (define shape (vector->immutable-vector (make-vector n d)))
               (define new-is (vector->immutable-vector
                                (for/vector #:length n #:fill 0 ([i (in-vector is)]) i)))
               (unless (equal? is new-is) (error 'WAT_89))
               (hash-update hsh n
                 (lambda (v)
                   (for/fold ([v v]) ([is (in-permutations (vector->list is))])
                     (let ([is (apply vector-immutable is)])
                       (hash-update v is
                         (lambda (v2)
                           (match-define (list a2 n) v2)
                           (unless (eq? a a2) (error 'WAT_97))
                           (list a2 (add1 n)))
                         (list a 0)))))
                 (hash))))
           (define arrs
             (for/list ([n (in-range (add1 max-n))])
               (define shape (vector->immutable-vector (make-vector n d)))
               (define v (hash-ref stuff n (hash)))
               (build-array shape
                 (lambda (is)
                   (match-define (list a n) (hash-ref v is (list #'0 0)))
                   #`(*: #,n #,a)))))]
     #:with (arr ...)
     (for/list ([arr (in-list arrs)])
       #`(array: #,(array->vector* arr)))
     #:with norm #'(multi-var-taylor-ish (list arr ...))])
  (define-splicing-syntax-class multi-var-taylor-ish-term
    #:attributes (n d shape is a)
    [pattern (~seq b:expr!x v:xi^n ...)
             #:fail-when (check-duplicate (attribute v.i)) "bad syntax"
             #:attr n (sum (attribute v.n))
             #:attr d (add1 (apply max 0 (attribute v.i)))
             #:attr shape (vector->immutable-vector (make-vector (attribute n) (attribute d)))
             #:do [(define hsh-i->n (make-immutable-hash (map cons (attribute v.i) (attribute v.n))))]
             #:attr is (apply vector-immutable
                              (append*
                               (for/list ([i (in-range (attribute d))])
                                 (make-list (hash-ref hsh-i->n i 0) i))))
             #:with a #'b])
  (define-splicing-syntax-class +-multi-var-taylor-ish-term #:datum-literals (+ -)
    #:attributes (n d shape is a)
    [pattern (~seq + b:multi-var-taylor-ish-term) #:with a #'b.a #:attr n (attribute b.n)
             #:attr is (attribute b.is) #:attr shape (attribute b.shape)
             #:attr d (attribute b.d)]
    [pattern (~seq - -b:multi-var-taylor-ish-term) #:with a #'(-: -b.a) #:attr n (attribute -b.n)
             #:attr is (attribute -b.is) #:attr shape (attribute -b.shape)
             #:attr d (attribute -b.d)])
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

(define-match-expander multi-var-taylor-ish:
  (syntax-parser [(multi-var-taylor-ish: :multi-var-taylor-ish-seq)
                  #'norm])
  (syntax-parser [(multi-var-taylor-ish: :multi-var-taylor-ish-seq)
                  #'norm]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (multi-var-taylor-ish: 1) (const 1))
  (check-equal? (multi-var-taylor-ish: 1 + 1 x) (mx+b 1 1))
  (check-equal? (multi-var-taylor-ish: 1 + 1 x + 1 x ^ 2) (ax^2+bx+c 1 1 1))
  (check-equal? (multi-var-taylor-ish: 1 + 2 x + 3 y) (ax+by+c 2 3 1))
  (check-equal? (multi-var-taylor-ish: 1 + 2 x + 3 y
                                       + 5 x ^ 2 + 7 x y + 11 y ^ 2)
                (multi-var-taylor-ish (list (array: 1) (array: #[2 3])
                                            (array: #[#[10 7] #[7 22]]))))
  (check-match (multi-var-taylor-ish (list (array: 1) (array: #[2 3])
                                           (array: #[#[10 7] #[7 22]])))
               (multi-var-taylor-ish: a + b x + c y
                                      + d x ^ 2 + e x y + f y ^ 2)
               (equal? (list a b c d e f)
                       (list 1 2 3 5 7 11)))
  (check-equal? (multi-var-taylor-ish: 1 + 2 x + 3 y
                                       + 5 x ^ 2 + 7 x y + 11 y ^ 2
                                       + 13 x ^ 3 + 17 x ^ 2 y + 19 x y ^ 2 + 23 y ^ 3)
                (multi-var-taylor-ish (list (array: 1) (array: #[2 3])
                                            (array: #[#[10 7] #[7 22]])
                                            (array: #[#[#[(* 6 13) (* 2 17)]
                                                        #[(* 2 17) (* 2 19)]]
                                                      #[#[(* 2 17) (* 2 19)]
                                                        #[(* 2 19) (* 6 23)]]]))))
  )
