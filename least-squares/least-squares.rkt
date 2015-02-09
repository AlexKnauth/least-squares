#lang sweet-exp racket/base

(provide linear-least-squares
         quadratic-least-squares
         polynomial-least-squares
         best-polynomial
         exponential-least-squares/logy
         linear-least-squares-3d
         mx+b
         ax^2+bx+c
         (struct-out power-function)
         (struct-out c*e^ax)
         (struct-out multi-var-taylor-ish)
         ax+by+c
         ax+by+cz+d
         power-function:
         )

(require racket/match
         racket/list
         math/matrix
         math/array
         syntax/parse/define
         infix/infix-macro
         "utils.rkt"
         (for-syntax racket/base syntax/parse))
(module+ test (require rackunit))

(define-simple-macro (def∑ ∑ [pat-id:id ...] points:id)
  #:with [tmp:id ...] (generate-temporaries #'[pat-id ...])
  #:with app-id1 (syntax-local-introduce #'#%app)
  #:with ooo (quote-syntax ...)
  (define-simple-macro ∑[e:expr ooo]
    #:with [tmp ... app-id2] (syntax-local-introduce (quote-syntax [pat-id ... app-id1]))
    (for/sum ([p (in-list points)])
      (define-syntax app-id2
        (syntax-parser
          [(_ f:id i:expr) #'(cond [(app list? f) (app list-ref f i)]
                                   [else (app f i)])]
          [(_ . stuff) #'(app . stuff)]))
      (match-define (list tmp ...) p)
      (: e ooo))))

(define (linear-least-squares-2d points)
  (def∑ ∑ [xi yi] points)
  ;; D = ∑[([a*xi + b] - yi)^2]
  ;; ∂D/∂a = ∑[2*([a*xi + b] - yi)*xi] = ∑[2*(xi^2*a + xi*b - xi*yi)]
  ;; ∂D/∂b = ∑[2*([a*xi + b] - yi)*1]  = ∑[2*(xi*a + b - yi)]
  ;; 0 = ∑[xi^2*a + xi*b - yi] = ∑[xi^2]*a + ∑[xi]*b - ∑[xi*yi] = 0
  ;; 0 = ∑[xi*a + b - yi]      = ∑[xi]*a + n*b - ∑[yi]          = 0
  ;; ∑[xi^2]*a + ∑[xi]*b = ∑[xi*yi]
  ;;   ∑[xi]*a +     n*b = ∑[yi]
  (defmulti
    [n (length points)]
    [∑xi^2  ∑[xi ^ 2]]
    [∑xi    ∑[xi]]
    [∑xi*yi ∑[xi * yi]]
    [∑yi    ∑[yi]])
  ;; [[ ∑xi^2  ∑xi ]  . [[ a ]   = [[ ∑xi*yi ]
  ;;  [  ∑xi    n  ]]    [ b ]]     [  ∑yi   ]]
  (defmatrix M [[ ∑xi^2  ∑xi ]
                [  ∑xi    n  ]])
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (matrix [[ ∑xi*yi ]
                                   [  ∑yi   ]])))
  (match-define (matrix: [[a] [b]]) X)
  (mx+b a b))

(define (quadratic-least-squares points)
  (def∑ ∑ [xi yi] points)
  ;; D = ∑[([a*xi^2 + b*xi + c] - yi)^2]
  ;; ∂D/∂a = ∑[2*([a*xi^2 + b*xi + c] - yi)*xi^2] = ∑[2*(xi^4*a + xi^3*b + xi^2*c - xi^2*yi)]
  ;; ∂D/∂b = ∑[2*([a*xi^2 + b*xi + c] - yi)*xi]   = ∑[2*(xi^3*a + xi^2*b + xi*c - xi*yi)]
  ;; ∂D/∂c = ∑[2*([a*xi^2 + b*xi + c] - yi)*1]    = ∑[2*(xi^2*a + xi*b + c - yi)]
  ;; ∑[xi^4]*a + ∑[xi^3]*b + ∑[xi^2]*c = ∑[xi^2*yi]
  ;; ∑[xi^3]*a + ∑[xi^2]*b +   ∑[xi]*c = ∑[xi*yi]
  ;; ∑[xi^2]*a +   ∑[xi]*b +       n*c = ∑[yi]
  (defmulti
    [n (length points)]
    [∑xi^4    ∑[xi ^ 4]]
    [∑xi^3    ∑[xi ^ 3]]
    [∑xi^2    ∑[xi ^ 2]]
    [∑xi      ∑[xi]]
    [∑xi^2*yi ∑[xi ^ 2 * yi]]
    [∑xi*yi   ∑[xi * yi]]
    [∑yi      ∑[yi]])
  ;; [[ ∑xi^4  ∑xi^3  ∑xi^2 ]    [[ a ]    [[ ∑xi^2*yi ]
  ;;  [ ∑xi^3  ∑xi^2   ∑xi  ]  *  [ b ]  =  [  ∑xi*yi  ]
  ;;  [ ∑xi^2   ∑xi     n   ]]    [ c ]]    [   ∑yi    ])
  (defmatrix M [[ ∑xi^4  ∑xi^3  ∑xi^2 ]
                [ ∑xi^3  ∑xi^2   ∑xi  ]
                [ ∑xi^2   ∑xi     n   ]])
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (matrix [[ ∑xi^2*yi ]
                                   [  ∑xi*yi  ]
                                   [   ∑yi    ]])))
  (match-define (matrix: [[a] [b] [c]]) X)
  (ax^2+bx+c a b c))

(define (polynomial-least-squares n points)
  (def∑ ∑ [xi yi] points)
  (define |(list ∑xi^(2n) ∑xi^(2n-1) ... ∑xi^0)|
    (for/list ([k (in-range {2 * n} -1 -1)])
      ∑[xi ^ k]))
  (define |(list ∑xi^n*yi ∑xi^(n-1)*yi ... ∑xi^0*yi)|
    (for/list ([k (in-range n -1 -1)])
      ∑[xi ^ k * yi]))
  (define M-list* (for/list ([row-num (in-range (add1 n))])
                    (for/list ([col-num (in-range (add1 n))])
                      (list-ref |(list ∑xi^(2n) ∑xi^(2n-1) ... ∑xi^0)|
                                (+ row-num col-num)))))
  (define M (list*->matrix M-list*))
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (->col-matrix
                           (for/list ([row-num (in-range (add1 n))])
                             (list-ref |(list ∑xi^n*yi ∑xi^(n-1)*yi ... ∑xi^0*yi)| row-num)))))
  (define X-list (matrix->list X))
  (power-function
   (for/hash ([k (in-range n -1 -1)]
              [v (in-list X-list)])
     (values k v))))

(define (best-polynomial points)
  (polynomial-least-squares (sub1 (length points)) points))

(define (exponential-least-squares/logy points)
  (define points/logy
    (for/list ([p (in-list points)])
      (match-define (list x y) p)
      (list x (log y))))
  (match-define (mx+b m b) (linear-least-squares points/logy))
  ;; e^(mx+b) = e^b*e^mx
  (c*e^ax (exp b) m))

(define (linear-least-squares-0d points)
  (multi-var-taylor-ish '()))

(define (linear-least-squares-1d points)
  (def∑ ∑ [yi] points)
  (define n (length points))
  (define a {∑[yi] / n})
  (multi-var-taylor-ish (list (array a))))

(define (linear-least-squares-3d points)
  (def∑ ∑ [xi yi zi] points)
  ;; D = ∑[(a*xi + b*yi + c - zi)^2]
  ;; ∂D/∂a = 2*∑[xi*(a*xi + b*yi + c - zi)]
  ;; ∂D/∂b = 2*∑[yi*(a*xi + b*yi + c - zi)]
  ;; ∂D/∂c = 2*∑[ 1*(a*xi + b*yi + c - zi)]
  ;;  ∑[xi^2]*a + ∑[xi*yi]*b + ∑[xi]*c = ∑[xi*zi]
  ;; ∑[xi*yi]*a +  ∑[yi^2]*b + ∑[yi]*c = ∑[yi*zi]
  ;;    ∑[xi]*a +    ∑[yi]*b +     n*c = ∑[zi]
  (defmulti
    [n (length points)]
    [∑xi^2  ∑[xi ^ 2]]
    [∑xi*yi ∑[xi * yi]]
    [∑xi    ∑[xi]]
    [∑yi^2  ∑[yi ^ 2]]
    [∑yi    ∑[yi]]
    [∑xi*zi ∑[xi * zi]]
    [∑yi*zi ∑[yi * zi]]
    [∑zi    ∑[zi]])
  ;; [[ ∑xi^2   ∑xi*yi  ∑xi ]    [[ a ]    [[ ∑xi*zi ]
  ;;  [ ∑xi*yi  ∑yi^2   ∑yi ]  *  [ b ]  =  [ ∑yi*zi ]
  ;;  [  ∑xi     ∑yi     n  ]]    [ c ]]    [  ∑zi   ]]
  (defmatrix M [[ ∑xi^2   ∑xi*yi  ∑xi ]
                [ ∑xi*yi  ∑yi^2   ∑yi ]
                [  ∑xi     ∑yi     n  ]])
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (matrix [[ ∑xi*zi ]
                                   [ ∑yi*zi ]
                                   [  ∑zi   ]])))
  (match-define (matrix: [[a] [b] [c]]) X)
  (ax+by+c a b c))

(define (linear-least-squares-4d points)
  (def∑ ∑ [x y z w] points)
  ;; D = ∑[(a*x + b*y + c*z + d - w)^2]
  ;; ∂D/∂a = 2*∑[x*(a*x + b*y + c*z + d - zi)]
  ;; ∂D/∂b = 2*∑[y*(a*x + b*y + c*z + d - zi)]
  ;; ∂D/∂c = 2*∑[z*(a*x + b*y + c*z + d - zi)]
  ;; ∂D/∂d = 2*∑[1*(a*x + b*y + c*z + d - zi)]
  ;; ∑[x^2]*a + ∑[x*y]*b + ∑[x*z]*c + ∑[x]*d = ∑[x*w]
  ;; ∑[x*y]*a + ∑[y^2]*b + ∑[y*z]*c + ∑[y]*d = ∑[y*w]
  ;; ∑[x*z]*a + ∑[y*z]*b + ∑[z^2]*c + ∑[z]*d = ∑[z*w]
  ;;   ∑[x]*a +   ∑[y]*b +   ∑[z]*c +    n*d = ∑[w]
  (defmulti
    [n (length points)]
    [∑x^2 ∑[x ^ 2]]
    [∑y^2 ∑[y ^ 2]]
    [∑z^2 ∑[z ^ 2]]
    [∑x*y ∑[x * y]]
    [∑x*z ∑[x * z]]
    [∑y*z ∑[y * z]]
    [∑x*w ∑[x * w]]
    [∑y*w ∑[y * w]]
    [∑z*w ∑[z * w]]
    [∑x   ∑[x]]
    [∑y   ∑[y]]
    [∑z   ∑[z]]
    [∑w   ∑[w]])
  ;; [[ ∑x^2  ∑x*y  ∑x*z  ∑x ]    [[ a ]    [[ ∑x*w ]
  ;;  [ ∑x*y  ∑y^2  ∑y*z  ∑y ]  .  [ b ]  =  [ ∑y*w ]
  ;;  [ ∑x*z  ∑y*z  ∑z^2  ∑z ]     [ c ]     [ ∑z*w ]
  ;;  [  ∑x    ∑y    ∑z   n  ]]    [ d ]]    [  ∑w  ]]
  (defmatrix M [[ ∑x^2  ∑x*y  ∑x*z  ∑x ]
                [ ∑x*y  ∑y^2  ∑y*z  ∑y ]
                [ ∑x*z  ∑y*z  ∑z^2  ∑z ]
                [  ∑x    ∑y    ∑z   n  ]])
  (define M^-1 (matrix-inverse M))
  (define X (matrix* M^-1 (matrix [[ ∑x*w ]
                                   [ ∑y*w ]
                                   [ ∑z*w ]
                                   [  ∑w  ]])))
  (match-define (matrix: [[a] [b] [c] [d]]) X)
  (ax+by+cz+d a b c d))

(define (linear-least-squares points)
  (def∑ ∑ [x ... w] points)
  (define n (apply max 0 (map length points)))
  (match n
    [0 (linear-least-squares-0d points)]
    [1 (linear-least-squares-1d points)]
    [2 (linear-least-squares-2d points)]
    [3 (linear-least-squares-3d points)]
    [4 (linear-least-squares-4d points)]
    [n (define M-lst
         (for/list ([r (in-range n)])
           (for/list ([c (in-range n)])
             (cond [(and (< r (sub1 n)) (< c (sub1 n)))
                    ∑(x[r] * x[c])]
                   [(and (= r (sub1 n)) (< c (sub1 n)))
                    ∑(x[c])]
                   [(and (< r (sub1 n)) (= c (sub1 n)))
                    ∑(x[r])]
                   [else
                    ∑(1)]))))
       (define M (list*->matrix M-lst))
       (define M^-1 (matrix-inverse M))
       (define X (matrix* M^-1 (list*->matrix
                                (for/list ([row-i (in-range n)])
                                  (list
                                   (cond [(< row-i (sub1 n))
                                          (for/sum ([p (in-list points)])
                                            (* (list-ref p row-i) (list-ref p (sub1 n))))]
                                         [else
                                          (for/sum ([p (in-list points)])
                                            (list-ref p (sub1 n)))]))))))
       (match-define (array: #[#[a] ... #[c]]) X)
       (multi-var-taylor-ish
        (list (array c)
              (list*->array a real?)))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (linear-least-squares '([0 0] [1 1] [2 2])) (mx+b 1 0))
  (check-equal? (linear-least-squares '([0 0] [1 2] [2 1])) (mx+b 1/2 1/2))
  (check-equal? (linear-least-squares '([0 1] [2 1] [3 4])) (mx+b 6/7 4/7))
  (check-equal? (linear-least-squares '([0 0] [1 2] [2 1] [3 4])) (mx+b 11/10 1/10))
  (check-equal? (linear-least-squares '([1 1] [2 3] [3 2])) (mx+b 1/2 1))
  (check-equal? (quadratic-least-squares '([0 0] [1 1] [2 4])) (ax^2+bx+c 1 0 0))
  (check-equal? (quadratic-least-squares '([0 1] [-1 0] [2 -3])) (ax^2+bx+c -1 0 1))
  (check-equal? (quadratic-least-squares '([0 1] [2 1] [3 4])) (ax^2+bx+c 1 -2 1))
  (check-match (polynomial-least-squares 1 '([0 0] [1 1] [2 2])) (power-function: x))
  (check-match (polynomial-least-squares 1 '([0 0] [1 2] [2 1])) (power-function: 1/2 x + 1/2))
  (check-match (polynomial-least-squares 2 '([0 0] [1 1] [2 4])) (power-function: 1 x^2))
  (check-match (polynomial-least-squares 2 '([0 1] [-1 0] [2 -3])) (power-function: - x^2 + 1))
  (check-equal? (linear-least-squares-3d '([0 0 0] [1 0 0] [0 1 0])) (ax+by+c 0 0 0))
  (check-equal? (linear-least-squares-3d '([0 0 0] [1 0 1] [0 1 0])) (ax+by+c 1 0 0))
  (check-equal? (linear-least-squares-3d '([0 0 0] [1 0 0] [0 1 1])) (ax+by+c 0 1 0))
  (check-equal? (linear-least-squares-3d '([0 0 0] [1 0 1] [0 1 1])) (ax+by+c 1 1 0))
  (check-equal? (linear-least-squares-3d '([0 0 3/2] [2 0 1] [0 1 2])) (ax+by+c -1/4 1/2 3/2))
  (check-equal? (linear-least-squares-4d '([0 0 0 3/2] [2 0 0 1] [0 1 0 2] [0 0 1 0]))
                (ax+by+cz+d -1/4 1/2 -3/2 3/2))
  (test-case "best-polynomial"
    (for ([n (in-range 1 15)])
      (define ps
        (for/list ([i (in-range n)])
          (list (exact-random/sgn 100) (exact-random/sgn 100))))
      (define f (best-polynomial ps))
      (for ([p (in-list ps)])
        (match-define (list x y) p)
        (check-equal? (f x) y))))
  (check-equal? (exponential-least-squares/logy '([-2 1/4] [-1 1/2] [0 1.0] [1 2] [2 4]))
                (c*e^ax 1.0 (log 2)))
  )
