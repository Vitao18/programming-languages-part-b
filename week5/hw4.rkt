
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

;; Function 3:
;; Receives a list of integers and a number. Returns the ith element of the list, where i is the remainder of number divided by the list length
(define (list-nth-mod xs number)
  (cond
    [(negative? number) (error "list-nth-mod: negative number")]
    [(null? xs)    (error "list-nth-mod: empty list")]
    [#t
     (car (list-tail xs
     (remainder number (length xs))))]))

;; Function 4:
;; Receives a stream and a numbers. Returns a list with the result of the nth values of the stream in order
(define (stream-for-n-steps stream number)
  (letrec ([f (lambda (stream counter)
                (let ([pr (stream)])
                  (if (= counter 0)
                      null
                      (cons (car pr) (f (cdr pr) (- counter 1))))))])
    (f stream number)))

;; Function 5:
;; Create a stream of natural numbers, but every multiple of 5 is negated
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (modulo x 5) 0)
                    (cons (* -1 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-the-dog
  (letrec ([x (lambda () (cons "dan.jpg" (lambda () y)))]
           [y (lambda () (cons "dog.jpg" (lambda () x)))])
  (lambda () (x))))

  