#lang racket/base

(require plot
         racket/list
         racket/function
         "least-squares.rkt"
         )

(define (exact-random n)
  (* (case (random 2) [(0) 1] [(1) -1])
     (+ (random n) (inexact->exact (random)))))

(define (random-points n within+-)
  (for/list ([i (in-range n)])
    (list (exact-random within+-) (exact-random within+-))))

(define (my-plot . args)
  (plot (cons (axes) (filter-not void? (flatten args)))))

(define (plot-linear-least-squares n)
  (define ps (random-points n 10))
  (my-plot (function (linear-least-squares ps) -10 10)
           (points ps)))

(define (plot-perfect-polynomial n #:both? [both? #f])
  (define rect
    (rectangles (list (list (ivl -10 10) (ivl -10 10)))))
  (define ps (random-points n 10))
  (my-plot rect (axes)
           (when both? (inverse (best-polynomial (map reverse ps)) -10 10 #:color "gray"))
           (function (best-polynomial ps) -10 10)
           (points ps)))

