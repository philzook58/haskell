(define (square x) (* x x))
(square 2)
(square 4)

(string-append "hello " "world")

(define (pow x y) (if (= y 0) 1 (* x (pow x (- y 1)))))

(pow 2 3)
(pow 3 4)

(define (map* f x) (if (eq? x '()) '() (cons (f (car x)) (map* f (cdr x)))))

(map* square '(1 2 3))
(map* (lambda (x) (* 2 x)) '(1 2 3))
(map square '(1 2 3))
;(reduce-left + 0 '(1 2 3))
(print-expression 'fred)
