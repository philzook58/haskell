#lang racket


(define (eval exp env)
  (case (car exp)
    ['plus (foldr + 0 (cdr exp))])
  )

; (eval `(plus 1 2 3) '())