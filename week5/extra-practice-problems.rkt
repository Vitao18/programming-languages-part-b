#lang racket

(define (palindromic numbers)
  (letrec ([f (lambda (start end)
               (if (< start (length numbers))
                   (cons (+ (list-ref numbers start) (list-ref numbers end)) (f (+ start 1) (- end 1)))
                   null))])
  (f 0 (- (length numbers) 1))))

(define fibonacci
  (letrec ([f (lambda (a b) (cons a (lambda () (f b (+ a b)))))])
    (lambda () (f 0 1))))
                    
(define (stream-until f s)
  (if (f (car (s)))
      (stream-until f (cdr (s)))
      #f))

(define (stream-map f s)
  (lambda () (cons (f (car (s))) (stream-map f (cdr (s))))))

(define (stream-zip s1 s2)
  (lambda () (cons (cons (car (s1)) (car (s2))) (stream-zip (cdr (s1)) (cdr (s2))))))

(define (pack n s)
  (letrec ([f (lambda (n s)
                (cons
                 (if (= n 0)
                    null
                    (cons (car (s)) (f (- n 1) (cdr (s)))))
                      (lambda () (cdr (s)))))])
    (f n s)))