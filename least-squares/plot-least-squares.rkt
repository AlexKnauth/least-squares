#lang sweet-exp racket/base

provide plot-linear-least-squares
        plot-quadratic-least-squares
        plot-polynomial-least-squares
        plot-best-polynomial
        plot-exponential-least-squares/logy

require plot
        racket/match
        racket/list
        "least-squares.rkt"
        "utils.rkt"

define (my-plot . args)
  (plot list*(axes() tick-grid() filter-not(void? flatten(args))))

define (my-plot3d . args)
  (plot3d list*(filter-not(void? flatten(args))))

define (plot-linear-least-squares ps)
  define f (linear-least-squares ps)
  match procedure-arity(f)
    [0 (define y f())
       (my-plot (function (λ (x) y) #:label (function-struct->string f #:y "y" #:xs '("x")))
                (points ps))]
    [1 (my-plot (function f #:label (function-struct->string f #:y "y" #:xs '("x")))
                (points ps))]
    [2 (my-plot3d (surface3d f #:label (function-struct->string f #:y "z" #:xs '("x" "y")))
                  (points3d ps))]

define (plot-quadratic-least-squares ps)
  define f (quadratic-least-squares ps)
  (my-plot (function f #:label (function-struct->string f))
           (points ps))

define (plot-polynomial-least-squares n ps)
  define f (polynomial-least-squares n ps)
  (my-plot (function f #:label (function-struct->string f))
           (points ps))

define (plot-best-polynomial ps)
  define f (best-polynomial ps)
  (my-plot (function f #:label (function-struct->string f))
           (points ps))

define (plot-exponential-least-squares/logy ps)
  define f (exponential-least-squares/logy ps)
  (my-plot (function f #:label (function-struct->string f))
           (points ps))


define (random-points n within+-)
  for/list ([i (in-range n)])
    list(exact-random/sgn(within+-) exact-random/sgn(within+-))


define (plot-linear-least-squares/random n)
  define ps (random-points n 10)
  (my-plot (function (linear-least-squares ps) -10 10)
           (points ps))

define (plot-best-polynomial/random n #:both? [both? #f])
  define rect
    (rectangles list(list(ivl[-10 10] ivl[-10 10])))
  define ps (random-points n 10)
  (my-plot rect (axes)
           (when both? (inverse (best-polynomial (map reverse ps)) -10 10 #:color "gray"))
           (function (best-polynomial ps) -10 10)
           (points ps))

