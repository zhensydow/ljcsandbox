#lang scheme
(define (makelist a b) (lambda (m) (m a b)))

(define (head l) (l (lambda (a b) a)))

(define (tail l) (l (lambda (a b) b)))

(head (tail (tail (makelist 1 (makelist 2 (makelist 3 4))))))