
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