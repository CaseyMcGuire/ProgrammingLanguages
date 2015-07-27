
;#lang racket

;(provide (all-defined-out)) ;; so we can put tests in a second file

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

