#lang racket/base

(provide multi-var-taylor-ish-sum)

(require racket/match
         racket/list
         math/base
         math/array
         syntax/parse/define
         "multi-var-taylor-ish.rkt"
         "../utils.rkt"
         )
(module+ test
  (require rackunit))

(define (multi-var-taylor-ish-sum . args)
  (define arrs
    (append* (for/list ([f (in-list args)])
               (match-define (multi-var-taylor-ish lst) f)
               lst)))
  (define arity (apply max 0 (map arr->arity arrs)))
  (define max-order (apply max 0 (map arr->order arrs)))
  (define lst
    (for/list ([n (in-range (add1 max-order))])
      (define shape (vector->immutable-vector (make-vector n arity)))
      (define arrs/n
        (for/list ([arr (in-list arrs)] #:when (= (arr->order arr) n))
          arr))
      (build-array shape
        (lambda (is)
          (sum (for/list ([arr (in-list arrs/n)])
                 (array-ref/else arr is (λ () 0))))))))
  (multi-var-taylor-ish (multi-var-taylor-ish-lst-remove-zeros-from-end lst)))

(define (arr->arity arr)
  (define shape (array-shape arr))
  (cond [(zero? (vector-length shape)) 0]
        [else (define n (vector-ref shape 0))
              (unless (for/and ([other-n (in-vector shape)]) (= other-n n))
                (error 'arr->arity "wrong shape for array\n  shape: ~v\n  array: ~v" shape arr))
              n]))

(define (arr->order arr)
  (vector-length (array-shape arr)))

(define (multi-var-taylor-ish-lst-remove-zeros-from-end lst)
  (let loop ([lst '()] [rev-lst (reverse lst)])
    (match rev-lst
      [(list) lst]
      [(list-rest fst rst)
       (cond [(array-andmap zero? fst) (loop lst rst)]
             [else (let loop ([lst (cons fst lst)] [rev-lst rst])
                     (match rev-lst
                       [(list) lst]
                       [(list-rest fst rst) (loop (cons fst lst) rst)]))])])))

(module+ test
  (define f+ multi-var-taylor-ish-sum)
  (define-simple-macro (defrandom id:id ...)
    (begin (define id (random)) ...))
  (defrandom a b c d e)
  (check-equal? (f+) zero-f)
  (check-equal? (f+ (const a)) (const a))
  (check-equal? (f+ (mx+b a b)) (mx+b a b))
  (check-equal? (f+ (mx+b a b) (const c)) (mx+b a (+ b c)))
  (check-equal? (f+ (ax^2+bx+c a b c) (mx+b d e)) (ax^2+bx+c a (+ b d) (+ c e)))
  (check-equal? (f+ (multi-var-taylor-ish (list (array a) (array #[a b c])))
                    (multi-var-taylor-ish (list (array a) (array #[b]) (array #[#[c]]))))
                (multi-var-taylor-ish (list (array (* 2 a)) (array #[(+ a b) b c])
                                            (array #[#[c 0 0] #[0 0 0] #[0 0 0]]))))
  )
