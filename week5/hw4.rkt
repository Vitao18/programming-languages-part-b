
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Function 1:
;; Creates a list if numbers starting from low until high, with step = stride
(define (sequence low high stride)
  (cond
    [(> low high) null]
    [(= low high) (cons low null)]
    [#t           (cons low (sequence (+ low stride) high stride))]))

;; Function 2:
;; Receives a list of words and a string. Return a copy of the list with every word appended by suffix string
(define (string-append-map xs suffix)
  (map (lambda (word) (string-append word suffix)) xs)) 