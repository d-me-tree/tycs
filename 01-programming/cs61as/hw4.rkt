#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

;; Assuming a is min and b in max of the interval
(define (upper-bound interval)
  (cdr interval))

;; Alternative definition
;(define (upper-bound interval)
;  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (car interval))

; SICP 2.8 - Define sub-interval

;; The minimum value would be the smallest possible value
;; of the first  minus the largest of the second.
;; The maximum would be the largest of the first minus
;; the smallest of the second.
(define (sub-interval x y)
  (make-interval (abs (- (lower-bound x) (upper-bound y)))
                 (abs (- (upper-bound x) (lower-bound y)))))

; SICP 2.10 - Modify div-interval

(define (spans-zero? interval)
  (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "The divisor interval spans 0!")
      (mul-interval x 
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y))))))


;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(define (make-center-percent c tol)
  (make-center-width c (* c (/ tol 100))))

; SICP 2.17 - Define last-pair

(define (last-pair lst)
  (if (= (length lst) 1)
      (list (car lst))
      (last-pair (cdr lst))))

; SICP 2.20 - Define same-parity

(define (same-parity n . rest)
  (define (is-even? x)
    (= (remainder x 2) 0))
  (define (is-odd? x)
    (not (is-even? x)))
  (append (list n)
          (if (is-even? n)
              (filter is-even? rest)
              (filter is-odd? rest))))  

; SICP 2.22 - Write your explanation in the comment block:

#|
cons adds an element to the beginning of a list
Louis needs to use append to make his procedure work
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  (map (lambda (item) (if (list? item)
                          (substitute item old new)
                          (if (equal? item old) new item)))
       lst))
      

; Exercise 3 - Define my-substitute2

(define (substitute2 lst old new)
  (define (get-ref el lst)
    (if (equal? (car lst) el)
        0
        (+ 1 (get-ref el (cdr lst)))))
  (map (lambda (item) (if (list? item)
                          (substitute2 item old new)
                          (if (member? item old)
                              (list-ref new (get-ref item old))
                              item)))
       lst))

; Exercise 4 - Define cxr-function

(define (cxr-function wd)
  (define (wd-to-list wd)
    (cond [(= (count wd) 1) null]
          [(equal? (first wd) 'a) (cons car (wd-to-list (bf wd)))]
          [(equal? (first wd) 'd) (cons cdr (wd-to-list (bf wd)))]
          [else (wd-to-list (bf wd))]))
  (lambda (x)
    (foldr (lambda (f v) (f v)) x (wd-to-list wd))))  

; test
(define a
  (cons
   ; car
     (cons
      ; car
        (cons
         ; car
           1
         ; cdr
           (cons
            ; car
              (cons 2 3)
            ; cdr
              4
            )
         )
      ; cdr
        5
      )
   ; cdr
     6
   ))

; ((cxr-function 'cadaar) a)  --> '(2 . 3)
; equivalent to (car (cdr (car (car a))))

; Exercise 5
; Hint: http://inst.eecs.berkeley.edu/~cs61as/library/church-hint

; Exercise 6 - Define my-reverse

(define (my-reverse lst)
  (foldl cons '() lst))