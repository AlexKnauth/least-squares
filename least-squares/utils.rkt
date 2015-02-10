#lang racket/base

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require racket/match
         math/array
         math/matrix
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     unstable/syntax
                     syntax/parse/define
                     (for-syntax racket/base
                                 )))

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

(begin-for-syntax
  (define-simple-macro (make-var-like/pat replace-stx:expr pat:expr)
    (let ([trans (set!-transformer-procedure (make-variable-like-transformer replace-stx))])
      (lambda (stx)
        (syntax-parse stx
          [pat (trans stx)])))))

(define (exact-random n)
  (+ (random n) (inexact->exact (random))))

(define (exact-random/sgn n)
  (define sgn (case (random 2) [(0) 1] [(1) -1]))
  (* sgn (exact-random n)))


