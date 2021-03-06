#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define substitute

(define (substitute sent old-word new-word)
  (if (empty? sent)
      '()
      (se (if (equal? (first sent) old-word)
              new-word
              (first sent))
          (substitute (bf sent) old-word new-word))))


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: #<procedure>

((lambda (x) (+ x 3)) 7)
-> returns: 10

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns: 10

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns: 10

(define (square x) (* x x)) 
(square 5)
-> returns:

(define square (lambda (x) (* x x))) 
(square 5)
-> returns 25

(define (try f) (f 3 5)) 
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


; Exercise 3
#|

Number of arguments g has: 0

Type of value returned by g: #<procedure>

(define (g) (lambda (x) (+ 2 x)))

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5
(define f1 'variable)
(define (f2) '(procedure with 0 arguments))
(define (f3 arg) (se '(procedure that takes one arg) arg '(in this case)))
(define (f4) (lambda () '(HOF with 0 arguments)))
(define (f5) (lambda () (lambda (x) (se '(HOF arg) x))))

; Exercise 5 - Try out the expressions

(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns: 3

2. ((t (t add1)) 0) returns: 9

3. (((t t) add1) 0) returns: 27 (add1 ^ 3)

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns: 3

2. ((t (t s)) 0) returns: 9

3. (((t t) s) 0) returns: 27

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  (lambda (x) (equal? x wd)))

; Exercise 8 - SICP exercises

; SICP 1.31a

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (estimate-pi)
  ; https://en.wikipedia.org/wiki/Wallis_product
  (* 2.0 (product
        (lambda (x) (* (/ (* 2 x) (- (* 2 x) 1)) (/ (* 2 x) (+ (* 2 x) 1))))
        1
        (lambda (x) (+ x 1))
        1000)))

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (my-accumulate combiner null-value term (next a) next b))))

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  (my-accumulate + 0 term a next b))

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  (my-accumulate * 1 term a next b))


; SICP 1.33

(define (filtered-accumulate combiner null-value term a next b pred)
  (cond [(> a b) null-value]
        [(pred a) (combiner (term a)
                            (filtered-accumulate
                             combiner null-value term (next a) next b pred))]
        [else (filtered-accumulate
               combiner null-value term (next a) next b pred)]))

(define (sum-sq-prime a b)
  (filtered-accumulate + 0 square a (lambda (x) (+ x 1)) b prime?))  

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (filtered-accumulate
   *
   1
   (lambda (x) x)
   1  ; starting value
   (lambda (x) (+ x 1))
   (- n 1)  ; we only want to consider ints LESS than n
   (lambda (x) (rel-prime? n x))))

; SICP 1.40 - Define cubic

(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

; SICP 1.41 - Define double

(define (double proc)
  (lambda (x) (proc (proc x))))

; SICP 1.43 - Define repeated

(define (my-repeated proc n)
  (if (= n 0)
      (lambda (x) x)
      (compose proc (my-repeated proc (- n 1)))))

; Exercise 9 - Define my-every

(define (my-every proc sent)
  (if (empty? sent)
      '()
      (se (proc (first sent)) (my-every proc (bf sent)))))

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns: `(pp uu rr pp ll ee)

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns: '(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns: '(76 24)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns: 'ooeee

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns: ""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns: ERROR Invalid arguments to MEMBER?:  purple aeiou

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns: '(purple)
|#
