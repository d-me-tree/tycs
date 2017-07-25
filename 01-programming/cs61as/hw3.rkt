#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond [(= n 0) a]
          [(even? n) (iter a (square b) (/ n 2))]
          [else (iter (* a b) b (- n 1))]))
  (iter 1 b n))

; Exericse 2 - Define phi

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

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

; Exercise 3 - Define cont-frac

;; Recursive version

;(define (cont-frac n d k)
;  (cond
;    [(= k 0) 0]
;    [(/ (n k) (+ (d k) (cont-frac n d (- k 1))))]))
; INCORRECT: produces Nk / (Dk + ...) vs N1 / (D1 + ...)

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))


;; Iterative version
(define (cont-frac-iter n d k)
  (define (iter result i)
    (if (= 0 i)
        result
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter 0 k))

(define (e k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (if (= (remainder i 3) 2)
                        (/ (+ i 1) 1.5)
                        1))
                  k)))

; Exercise 4 - Define next-perf

(define (next-perf n)
  (define (sum-of-factors k n)
    (cond [(= k 0) 0]
          [(= (remainder n k) 0) (+ k (sum-of-factors (- k 1) n))]
          [(sum-of-factors (- k 1) n)]))
  (define (is-perfect? n)
    (and (> n 0) (= (sum-of-factors (- n 1) n) n)))
  (if (is-perfect? n)
      n
      (next-perf (+ n 1))))
  

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

(cc 0 '()) -- > 0, which is incorrect
we want to count 0 as 1 way to make change

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter:
product * b ^ counter == b ^ n

|#
