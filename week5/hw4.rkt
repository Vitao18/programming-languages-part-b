
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

;; Function 6:
;; Create a stream of two strings: dan.jpg and dog.jpg.
(define dan-the-dog
  (letrec ([x (lambda () (cons "dan.jpg" (lambda () y)))]
           [y (cons "dog.jpg" x)])
  (lambda () (x))))

;; Function 7:
;; Given a stream returns a new stream with the ith value v of the stream being replaced by a pair like (0 . v)
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (let ([pr (s)])
                  (cons (cons 0 (car pr)) (lambda () (f (cdr pr))))))])
    (lambda () (f s))))

;; Function 8:
;; Given two lists xs and ys, creates a stream with nth-mod element of xs and ys appended together
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons
                 (cons (list-nth-mod xs x) (list-nth-mod ys x))
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))
;; Function 9:
;; Given a vector vec and a value v, iterates through the vector vec and check if each element is a pair. Returns a pair p if the first element of the pair
;; is v. Returns f if none of the elements of the vector vec are a pair or if none of the first elements if v.
(define (vector-assoc v vec)
  (letrec ([f (lambda (i)
    (cond
      [(equal? (vector-length vec) i) #f]
      [(not (pair? (vector-ref vec i))) (f (+ i 1))]
      [(equal? (car (vector-ref vec i)) v) (vector-ref vec i)]
      [else (f (+ i 1))]))])
    (f 0)))

