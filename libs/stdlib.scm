;; Lagoon Standard Library

(define (not x) (if x #f #t))
;;(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . objs) objs)
(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (compose p1 p2) (lambda (arg) (p1 (p2 arg))))
(define cadr (compose car cdr))
(define cdar (compose cdr car))

(define abs (lambda [n] (if (< n 0) (- 0 n) n)))
