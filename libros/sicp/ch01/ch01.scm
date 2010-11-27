#lang racket

;Exercise 1.9
(define (inc a)
  (+ a 1))

(define (dec b)
  (- b 1))

;(define (sum a b)
;  (if (= a 0)
;      b
;      (inc (sum (dec a) b)))) 

;Exercise 1.10
(define (ack x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ack (- x 1)
                 (ack x (- y 1))))))

;(define (f n) (ack 0 n))
;(define (g n) (ack 1 n))
;(define (h n) (ack 2 n))
;(define (k n) (* 5 n n))

;Exercise 1.11
(define (fr n)
  (cond
    ((< n 3) n)
    (else (+ (fr (- n 1)) (* 2 (fr (- n 2))) (* 3 (fr (- n 3)))))))

(define (fi n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if
   (= count 0)
   c
   (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

;Exercise 1.12
(define (pascal n m)
  (cond
    ((= n 0) 1)
    ((= n m) 1)
    (else (+ (pascal (- n 1) (- m 1)) (pascal n (- m 1))))))

;Exercise 1.13
(define phi
  (/ (+ 1 (sqrt 5)) 2))

(define psi
  (/ (- 1 (sqrt 5)) 2))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if
   (= count 0)
   b
   (fib-iter (+ a b) a (- count 1))))

(define (power p n)
  (if
   (= n 1)
   p
   (* p (power p (- n 1)))))

(define (fib-psi n)
  (/ (- (power phi n) (power psi n)) (sqrt 5)))

;Exercise 1.16
(define (expn b n)
  (expn-f 1 b n))

(define (expn-f a b n)
  (cond
    ((= 0 n) a)
    ((even? n) (expn-f a (* b b) (/ n 2)))
    (else (expn-f (* a b) b (- n 1)))))

(define (square n) (* n n))

(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
     (remainder (square (expmod base (/ exp 2) m)) m))
    (else
     (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

;Exercise 1.29
(define (cube n) (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (range a b)
  (if (= a b)
      '()
      (cons a (range (+ a 1) b))))

(define (suma lista)
  (if (null? lista)
      0
      (+ (car lista) (suma (cdr lista)))))

(define (simpsons f a b n)
  (define (const k)
    (if (even? k) 2 4))
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (elem k) (* (const k) (y k)))
  (define lista
    (cons (y 0) (cons (y n) (map elem (range 1 (- n 1))))))
  (* (/ h 3) (suma lista)))

;Exercise 1.30
(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;Exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product (lambda (k) k) 1 (lambda (k) (+ k 1)) n))

(define (pi-aprox n)
  (define (term k) (/ (* (* 2 k) (* 2 (+ k 1))) (* (+ (* 2 k) 1) (+ (* 2 k) 1))))
  (* 4 (product term 1 (lambda (k) (+ k 1)) n)))

;Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;Exercise 1.33
(define (filtered-accumulate pred combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (pred (term a)) (combiner (term a)) result))))
  (iter a null-value))

;Exercise 1.34
(define (ff g)
  (g 2))

;Exercise 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;Exercise 1.36
(define (fx x) 
  (display x)
  (newline)
  (/ (log 1000) (log x)))

;Exercise 1.37
(define (cont-frac n d k)
  (define (iter a result)
    (if (= a 0)
        result
        (iter (- a 1) (/ (n a) (+ (d a) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (cf-golden k) (/ 1.0 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

;Exercise 1.38
(define (euler-exp n)
  (let ((k (+ n 1)))
    (if (= (remainder k 3) 0)
        (* 2 (/ k 3))
        1)))

(define (cf-e k) (+ 2 (cont-frac (lambda (i) 1.0) euler-exp k)))

;Exercise 1.39
(define (cf-tan x k)
  (define (d k) (- (* 2 k) 1))
  (define (n k)
    (if (< k 2)
        x
        (- 0 (* x x))))
  (cont-frac d n k))
