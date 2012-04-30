#lang racket/base
(require racket/mpair)

;Exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (mcons (mcar x) (append (mcdr x) y))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define x (mlist 'a 'b))
(define y (mlist 'c 'd))

(define z (append x y))

(define w (append! x y))

;Exercise 3.13
(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

;Exercise 3.14
(define (mistery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (mlist 'a 'b 'c 'd))

;Exercise 3.16
(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))

(define nox01 (mlist 'a 'b))
(define nox02 (mcons nox01 nox01))
(define nox03 (mcons (mlist 'a 'b) (mlist 'a 'b)))

;Exercise 3.17
(define (counted? x xs)
  (if (null? xs)
      #f
      (if (eq? x (mcar xs))
          #t
          (counted? x (mcdr xs)))))

(define (count-pairs2 x)
  (define table '())
  (define (count-pairs-temp x)
    (if (not (mpair? x))
        0
        (if (counted? x table)
            0
            (begin
              (set! table (mcons x table))
              (+ (count-pairs-temp (mcar x))
                 (count-pairs-temp (mcdr x))
                 1)))))
  (count-pairs-temp x))

;Exercise 3.18
(define (cicle? x)
  (define table '())
  (define (cicle?-temp x)
    (if (null? x)
        #f
        (if (counted? (mcar x) table)
            #t
            (begin
              (set! table (mcons (mcar x) table))
              (cicle?-temp (mcdr x))))))
  (cicle?-temp x))
        
;Exercise 3.20
(define (lcons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (lcar z) (z 'car))
(define (lcdr z) (z 'cdr))
(define (lset-car! z val)
  ((z 'set-car!) val)
  z)
(define (lset-cdr! z val)
  ((z 'set-cdr!) val)
  z)

(define lx (lcons 1 2))
(define lz (lcons lx lx))
