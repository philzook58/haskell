#lang racket

(define (repeated x n)
  (if (> n 0)
      (cons x (repeated x (- n 1)))
      '()
      ))

(define  (step n spaces) (list->string (append  (repeated #\space spaces)   (repeated #\# (- n spaces)) )))


(define (staircase n spaces)
  (if (>= spaces 0)
     
(begin
  (printf (step n spaces))
  (printf "\n")
  (staircase n (- spaces 1))
  )
(void)))




