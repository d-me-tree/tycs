#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
(require (planet dyoo/simply-scheme))

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

(define (fib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

(define sent '(1 2 3 4 5))

(define (sum-sent sent)
  (if (empty? sent)
      0
      (+ (first sent) (sum-sent (bf sent)))))

(define (count-ums sent)
  (if (empty? sent)
      0
      (+ (if (equal? (first sent) 'um) 1 0) (count-ums (bf sent)))))

;; count-ums using the "keep" pattern
;; (trace count-ums-v2)
;; (count-ums-v2 '(today um we are going to um talk about the um combining method))
(define (count-ums-v2 sent)
  (cond
    [(empty? sent) 0]
    [(equal? (first sent) 'um) (+ 1 (count-ums-v2 (bf sent)))]
    [else (count-ums-v2 (bf sent))]))

(define (countdown n)
  (if (= n 0)
      'blastoff!
      (se n (countdown (- n 1)))))

(define (numbers sent)
  (cond
    [(empty? sent) '()]
    [(number? (first sent)) (se (first sent) (numbers (bf sent)))]
    [else (numbers (bf sent))]))

(define (disjoint-pairs wd)
  (cond
    [(empty? wd) '()]
    [(= (count wd) 1) (se wd)]
    [else (se (word (first wd) (first (bf wd)))
              (disjoint-pairs (bf (bf wd))))]))

(define (disjoint-pairs-test  wd)
  (if (= (count wd) 1)
      '()
      (se (word (first wd) (first (bf wd)))
          (disjoint-pairs (bf (bf wd))))))

(define (square x) (* x x))

(define (largest-square total guess)
  (if (< (- total (square guess)) 0)
      (- guess 1)
      (largest-square total (+ guess 1))))

(define (sent-max sent)
  (if (= (count sent) 1)
      (count (first sent))
      (max (count (first sent)) (sent-max (bf sent)))))

(define (every-nth n sent)
  (every-nth-helper n n sent))

(define (every-nth-helper interval remaining sent)
  (cond
    [(empty? sent) '()]
    [(= remaining 1) (se (first sent) (every-nth-helper interval interval (bf sent)))]
    [else (every-nth-helper interval (- remaining 1) (bf sent))]))

(define (location wd sent)
  (if (> (location-helper wd sent) (count sent))
      #f
      (location-helper wd sent)))
         
(define (location-helper wd sent)
  (cond
    [(empty? sent) 1]
    [(equal? wd (first sent)) 1]
    [else (+ 1 (location-helper wd (bf sent)))]))

;; fixed-point
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
  (try first-guess))

;(define (sqrt x)
;  (fixed-point (lambda (y) (* (/ 1 2) (+ y (/ x y)))) 1.0))

(define (average-damp f)
  (lambda (y) (* 0.5 (+ y (f y)))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (iterate start improve good-enough?)
  (if (good-enough? start)
      start
      (iterate (improve start) improve good-enough?)))

(define (largest-square-v2 total guess)
  (iterate guess
           (lambda (x) (+ x 1))
           (lambda (x) (< total (square (+ x 1))))))
;  OR
;  (define (next x) (+ x 1))
;  (define (good-enough? x) (< total (square (next x))))
;  (iterate guess next good-enough?))