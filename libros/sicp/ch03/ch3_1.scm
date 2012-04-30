#lang racket/base
(require rnrs/base-6)

;Exercise 3.1
(define (mk-accumulator n)
  (lambda (inc)
    (begin
      (set! n (+ n inc))
      n)))

(define A (mk-accumulator 5))

;Exercise 3.2
(define (mk-monitored f)
  (let ((counter 0))
    (lambda (in)
      (cond ((eq? in 'how-many-calls?) counter)
            ((eq? in 'reset-count!) (set! counter 0))
            (else 
             (begin
               (set! counter (+ counter 1))
               (f in)))))))

;Exercise 3.3
;Exercise 3.4
(define (mk-account balance pass)
  (define tries 7)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (invalid . args)
    (begin
      (set! tries (- tries 1))
      (if (<= tries 0)
          (error "Call the police")
          "Incorrect password")))
  (define (dispatch password m)
    (if (eq? pass password)
        (begin
          (set! tries 7)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MK-ACCOUNT" m))))
        invalid))
  dispatch)

;Exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
           
(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((rx (random-in-range x1 x2))
          (ry (random-in-range y1 y2)))
      (pred rx ry)))
  (let ((factor (monte-carlo trials experiment))
        (area (* (- x2 x1) (- y2 y1))))
    (* area factor)))

(define (P1 x y)
  (<= (+ (expt (- x 5) 2) (expt (- y 7) 2)) (expt 3 2)))

(define (P2 x y)
  (<= (+ (expt x 2) (expt y 2)) 1))

(define (pi-estimate trials)
  (/ (estimate-integral P2 -2 2 -2 2 trials) 1))

;Exercise 3.6
(define mrand
  (let ((x 0))
    (define (generate)
      (let ((ret (mod (+ (* x 13) 7) 11)))
        (begin
          (set! x ret)
          ret)))
    (define (reset v)
      (set! x (mod v 11)))
    (lambda (m)
      (cond ((eq? m 'generate)
             (generate))
            ((eq? m 'reset)
             reset)
            (else
             (error "Unknow -- RAND" m))))))

;Exercise 3.7
(define (mk-clear-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MK-CLEAR-ACCOUNT" m))))
  dispatch)
  
(define (mk-password-account balance pass)
  (define account (mk-clear-account balance))
  (define (protect account pass)
    (define  tries 7)
    (define (invalid . args)
      (begin
        (set! tries (- tries 1))
        (if (<= tries 0)
            (error "Call the police")
            "Incorrect password")))
    (define (dispatch password m)
      (if (eq? pass password)
          (cond ((eq? m 'joint) (lambda (np) (protect account np)))
                (else
                 (begin
                   (set! tries 7)
                   (account m))))
          invalid))
    dispatch)
  (protect account pass))

(define (make-joint account pass newpass)
  ((account pass 'joint) newpass))

;Exercise 3.8
(define (mk-f inicial)
  (lambda (n)
    (begin
      (let ((old inicial))
        (set! inicial n)
        old))))

(define f (mk-f 0))