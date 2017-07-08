#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  (cond
    [(empty? sent) '()]
    [(member? (first sent) (bf sent)) (dupls-removed (bf sent))]
    [else (se (first sent) (dupls-removed (bf sent)))]))

; Exercise 2 - Define count-word

(define (count-word sent wd)
  (cond
    [(empty? sent) 0]
    [(equal? (first sent) wd) (+ 1 (count-word (bf sent) wd))]
    [else (count-word (bf sent) wd)]))

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here

|#

; Exercise 4 - Define squares

(define (squares sent)
  (define (square x) (* x x))
  (if (empty? sent)
      '()
      (se (square (first sent)) (squares (bf sent)))))

; Exercise 5 - Define switch

(define (switch sent)
  (if (equal? (first sent) 'you)
      (se 'I (switch-helper (bf sent)))
      (switch-helper sent)))

(define (switch-helper sent)
  (cond
    [(empty? sent) '()]
    [(or (equal? (first sent) 'I) (equal? (first sent) 'me))
     (se 'you (switch-helper (bf sent)))]
    [(equal? (first sent) 'you)
     (se 'me (switch-helper (bf sent)))]
    [else (se (first sent) (switch-helper (bf sent)))]))

; Exercise 6 - Define ordered?

(define (ordered? sent)
  (cond
    [(= (count sent) 1) #t]
    [(> (item 2 sent) (item 1 sent)) (ordered? (bf sent))]
    [else #f]))
      

; Exercise 7 - Define ends-e

(define (ends-e sent)
  (cond
    [(empty? sent) '()]
    [(equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent)))]
    [else (ends-e (bf sent))]))

; Exercise 8

;; (test-or 0) --> #t, but (test-or 1) --> inf loop
;; or is a special form in Racket
(define (test-or x)
  (define (y) (y))
  (or (= x 0) (= (y) 0)))

(define (new-or x y)
  (or x y))

;; now in both cases, (test-new-or 0) and (test-new-or 1), we get an inf loop
(define (test-new-or x)
  (define (y) (y))
  (new-or (= x 0) (= (y) 0)))

;; (test-and 0) --> inf loop, but (test-and 1) --> #f
;; and is a special form in Racket
(define (test-and x)
  (define (y) (y))
  (and (= x 0) (= (y) 0)))

#|

Your explanation here

|#
