
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
(define (cycle-lists xs ys)
  (letrec ([list-cycle (lambda (cur-list total-list)
                         (if (null? cur-list)
                             (list-cycle total-list total-list)
                             (cons (car cur-list) (lambda() (list-cycle (cdr cur-list) total-list)))))]
           [f (lambda (s1 s2)
                  (lambda () (cons (cons (car s1) (car s2)) (f ((cdr s1)) ((cdr s2))))))])
    (f (list-cycle xs xs) (list-cycle ys ys))))

; Write a function vector-assoc that a value v and a vector vec. It should behave like Racket's assoc library function
; except (1) it processes a vector (Racket's name for an array) instead of a list, (2) it allows vector elements not to be 
; pairs in which case it skips them, and (3) it always takes exactly two arguments. Process the vector elements in order 
; starting from 0.
(define (vector-assoc v vec)
  (letrec ([f (lambda (iter)
                (cond
                  ((= (vector-length vec) iter) #f)
                  ((and (pair? (vector-ref vec iter)) (equal? (car (vector-ref vec iter)) v)) (vector-ref vec iter))
                  (else (f (+ iter 1)))))])
    (f 0)))
                   
; Write a function cached-assoc that takes a list xs and a number n and returns a function that takes one argument v and 
; returns the same thing that (assoc v xs) would return. However, you should use an n-element cache of recent results to 
; possibly make this function faster than just calling assoc. The cache must be a Racket vector of length n that is created
; by the call to cached-assoc and used-and-possibly mutated each time the function returned by cached-assoc is called. 
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [iter 0] 
           [f (lambda (n)
                (letrec ([cached-result (vector-assoc n memo)])
                  (if cached-result
                      cached-result
                      (letrec ([list-result (assoc n xs)])
                        (if (not list-result)
                            list-result
                            (begin 
                              (print "In the cache")
                              (vector-set! memo iter list-result)
                              (if (= iter (- (vector-length memo) 1))
                                  (set! iter 0)
                                  (set! iter (+ iter 1)))
                              list-result))))))])
    f))
                      
; Define a macro that is used like (while-less e1 do e2) where e1 and e2 are expressions and while-less and do are syntax
; (keywords). The macro should do the following: 
; It evaluates e1 exactly once.
; It evaluates e2 at least once.
; It keeps evaluating e2 until and only until the result is not a number less than the result of the evaluation of e1.
; Assuming evaluation terminates, the result is #t.
; Assume e1 and e2 produce numbers; your macro can do anything or fail mysteriously otherwise. 
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([exp1 e1]
           [f (lambda (x) 
                (if (<= exp1 x)
                    #t
                    (f e2)))])
       (f e2))]))
       