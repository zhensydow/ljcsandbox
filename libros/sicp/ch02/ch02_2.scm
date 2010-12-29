#lang racket

;Exercise 2.17
(define (last-pair ll)
  (if (null? (cdr ll))
      (car ll)
      (last-pair (cdr ll))))

;Exercise 2.18
(define (reverse ll)
  (if (null? ll)
      ll
      (append (reverse (cdr ll)) (list (car ll)))))

;Exercise 2.19
(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins)) 0)
        (else (+ (cc amount (cdr coins))
                 (cc (- amount (car coins)) coins)))))

(define (same-parity i . ll)
  (if (odd? i)
      (cons i (filter odd? ll))
      (cons i (filter even? ll))))

;Exercise 2.21
(define (square x) (* x x))

(define (square-list1 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

;Exercise 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer 
                    (square (car things))))))
  (iter items '()))

(define (square-list5 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer 
                    (list (square (car things)))))))
  (iter items '()))

;Exercise 2.24

;Exercise 2.25
(define l1 '(1 3 (5 7) 9))
(define l2 '((7)))
(define l3 '(1 (2 (3 (4 (5 (6 7)))))))

;Exercise 2.26
(define lx (list 1 2 3))
(define ly (list 4 5 6))

;Exercise 2.27
(define (deep-reverse ll)
  (if (null? ll)
      '()
      (let ((nn (car ll)))
        (if (list? nn)
            (append (deep-reverse (cdr ll)) (list (deep-reverse nn)))
            (append (deep-reverse (cdr ll)) (list nn))))))
