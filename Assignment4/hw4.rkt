
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be numbers
; Further, assume stride is positive. sequence produces a list of numbers from low to high (including low
; and possibly high) separated by stride and in sorted order.

(define sequence
  (lambda (low high stride)
    (cond
      ((> low high) null)
      ((= low high) (cons low null))
      (else (cons low (sequence (+ low stride) high stride))))))



; Write a function string-append-map that takes a list of strings xs and a string suffix and returns a list of
; strings. Each element of the output should be corresponding element of the input appended with suffix (with no
; extra space between the element of the suffix).
;(define (string-append-map xs suffix)
(define (string-append-map xs suffix)
  (map (lambda (str)
         (string-append str suffix)) xs))

; Write a function list-nth-mod that takes a list xs and a number n. If the number is negative, terminate the 
; computation with (error "list-nth-mod: negative number"). Else if the list is empty, terminate the computation
; with (error "list-nth-mod: empty list"). Else return the ith list's length.
(define (list-nth-mod xs n)
  (cond
    ((< n 0) (error "list-nth-mod: negative number"))
    ((null? xs) (error "list-nth-mod: empty list"))
    (else (car (list-tail xs (remainder (length xs) n))))))

; Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding the first n
; values produced by s in order. Assume n is non-negative.

(define (stream-for-n-steps s n)
  (let ([s2 (s)])
    (if (= n 0)
        null
        (cons (car s2) (stream-for-n-steps (cdr s2) (- n 1))))))

; Write a stream funny-number-stream that is like the stream of natural numbers except numbers divisible by 5 are
; negated. 
(define funny-number-stream 
  (letrec ([f (lambda (x) 
                (if (= (remainder x 5) 0)
                    (cons (- x (* x 2)) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg" and "dog.jpg"
; (starting with "dan.jpg"). 
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (equal? x "dan.jpg")
                    (cons x (lambda () (f "dog.jpg")))
                    (cons x (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))
                    
; Write a function stream-add-zero that takes a stream s and returns another stream. If s would produce v for its
; ith element, then (stream-add-zero s) would produce the pair (0 . v) for its ith element. 
(define (stream-add-zero s)
  (letrec ([s2 (s)]
           [f (lambda ()
                (cons (cons 0 (car s2)) (lambda () ((stream-add-zero (cdr s2))))))])
     f))

; Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may or may not be the
; same length, but assume they are both non-empty. The elements produced produced by the stream are pairs where
; the first part is from xs and the second part is from ys. 