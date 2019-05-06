#lang scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Defining Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vhello "Hello world")

(define (fhello)
  "Hello world")

(define (hello name)
  (string-append "Hello " name "!"))

(define (sum3 a b c)
  (+ a b c))

;; Exercise 1
(define (inc a) (+ a 1))

(define (dec a) (- a 1))

;; Exercise 2
(define pi (* 4 (atan 1.0)))

(define (deg2rad d) (/ (* pi d) 180))
(define (rad2deg r) (/ (* 180 r) pi))

(define (dx vx t) (* vx t))

(define (freefall-time vy)
  (/ (* 2.0 vy) 9.8))

(define (distance v ang)
  (dx
   (* v (cos (deg2rad ang)))
   (freefall-time (* v (sin (deg2rad ang))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Branching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum-gp a0 r n)
  (* a0
     (if (= r 1)
	 n
	 (/ (- 1 (expt r n)) (- 1 r)))))

(define (abs v)
  (if (< v 0) (- 0 v) v))

(define (reciprocal v)
  (if (eq? v 0)
      #f
      (/ 1 v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Branching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
	  (begin
	    (close-input-port p)
	    (list->string (reverse ls1)))
	  (loop (cons c ls1) (read-char p))))))
