#lang racket
(require mechanics)

(+ (* pi (square 2)) 1)

(exp2 3)
;(up 1 2 3)
#|
(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

(print-expression (q 't))
(up (x t) (y t) (z t))
|#
