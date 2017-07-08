#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  (cond
    [(>= secs 86400) (se
                      (quotient secs 86400)
                      'DAYS
                      (describe-time (remainder secs 86400)))]
    [(>= secs 3600) (se
                     (quotient secs 3600)
                     'HOURS
                     (describe-time (remainder secs 3600)))]
    [(>= secs 60) (se
                   (quotient secs 60)
                   'MINUTES
                   (describe-time (remainder secs 60)))]
    [else (se
           secs
           'SECONDS)]))
    

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (cond
    [(empty? sent) '()]
    [(equal? (first sent) wd) (bf sent)]
    [else (se (first sent) (remove-once wd (bf sent)))]))

; Exercise 3 - Define differences
(define (differences nums)
  (if (= (count nums) 2)
      (- (first nums) (last nums))
      (se (- (first nums) (item 2 nums)) (differences (bf nums)))))

; Exercise 4 - Define location
(define (location small big)
  ; your code here
  (error "Not yet implemented")
  )

(define (loc0 s b) #f) ; (loc0 'a '())

(define (loc1 s b)
  (if (equal? (first b) s)
      1
      (loc0 s (bf b))))

(define (loc2 s b)
  (if (equal? (first b) s)
      1
      (loc1 s (bf b))))

; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent)) (initials (bf sent)))))

; Exercise 6 - Define copies
(define (copies num wd)
  (if (= num 0)
      '()
      (se wd (copies (- num 1) wd))))

; Exercise 7 - Define gpa
(define (gpa grades)
  ; your code here
  (error "Not yet implemented")
  )

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  ; your code here
  (error "Not yet implemented")
  )

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  ; your code here
  (error "Not yet implemented")
  )
