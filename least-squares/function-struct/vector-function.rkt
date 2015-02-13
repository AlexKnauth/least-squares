#lang racket/base

(provide vector-function)

(require racket/match
         racket/list
         racket/splicing
         kw-utils/arity+keywords
         "../utils.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require rackunit))

(struct super-vector-function (lst) #:transparent)

(define (make-vector-function lst)
  (define arity
    (cond [(empty? lst) 0]
          [else (apply arity-combine/or (map procedure-arity lst))]))
  (define constructor (get-vector-function-constructor arity))
  (constructor lst))

(splicing-local
    [;; sub-constructors : (HashTable Procedure-Arity [(Listof Procedure) -> Vector-Function])
     (define sub-constructors (make-hash))
     ;; vector-function : [Vector-Function Real * -> (Listof Real)]
     ;; will be passed as the procedure argument to prop:procedure
     (define (vector-function f . args)
       (match-define (super-vector-function lst) f)
       (for/list ([f (in-list lst)])
         (apply app/forget-arity f args)))
     ;; proc : [My-F Real * -> Real]
     (define proc vector-function)]
  (define (get-vector-function-constructor arity)
    (hash-ref! sub-constructors arity
      (lambda ()
        (struct vector-function super-vector-function () #:transparent
          #:property prop:procedure (procedure-reduce-arity proc (arity-add arity 1)))
        vector-function))))

(define-match-expander vector-function
  (syntax-parser
    [(vector-function lst-pat:expr)
     #'(super-vector-function lst-pat)])
  (make-var-like/pat #'make-vector-function (~or :id (:id :expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (procedure-arity (vector-function '())) 0)
  (define f2 (vector-function (list (λ (x) (+ x 1)) (λ (x y) (+ x y)))))
  (check-equal? (procedure-arity f2) '(1 2))
  (check-equal? (f2 0) '(1 0))
  (check-equal? (f2 0 3) '(1 3))
  (check-equal? (f2 1) '(2 1))
  (check-equal? (f2 1 3) '(2 4))
  )
