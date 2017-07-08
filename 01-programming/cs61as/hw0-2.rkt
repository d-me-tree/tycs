#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10

;2. Compound Expression (3 Atoms)
(- 12 2)

;3. Compound Expression (4 Atoms)
(+ 1 2 7)

;4. Compound Expression (1 Atom and 2 subexpressions)
(* 2 (+ 4 1))

;5. Any Other Kind Expression
(+ (* 3 6) (- 0 8))


;Exercise 1
(define (second wd)
  (item 2 wd))

;1. Define first-two
(define (first-two wd)
  (word (first wd) (second wd))
)

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y))
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word (first (first sent)) (first (second sent)))
)

;Exercise 2 - Define teen?
(define (teen? num)
  (and (>= num 13) (<= num 19)))
      

;Exercise 3 - Define indef-article
(define (indef-article wd)
  (se (if (member? (first wd) 'aeiou) 'an 'a) wd))

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (se (butlast sent) 'and (last sent)))

;Exercise 5 - Define query
(define (query sent)
  (se (second sent) (first sent) (bf (bf (bl sent))) (word (last sent) '?)))

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (+ (remainder (first time) 12) (if (equal? (second time) 'am) 0 12)))

(define (american-time time)
  (cond
    [(= time 0) '(12 am)]
    [(= time 12) '(12 pm)]
    [(< time 12) (se time 'am)]
    [else (se (remainder time 12) 'pm)]))
      
;Exercise 7 - Define describe-time
(define (describe-time secs)
  (cond
    [(>= secs 86400) (se (/ secs 86400.0) 'days)]
    [(>= secs 3600) (se (/ secs 3600.0) 'hours)]
    [(>= secs 60) (se (/ secs 60.0) 'minutes)]
    [else (se secs 'seconds)])) 

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective noun)
  (se (word adjective 'est) noun))

#|

Explanation here.

|#