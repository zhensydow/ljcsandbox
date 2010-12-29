#lang racket

;Exercise 2.1
(define (make-rat n d)
  (let* 
      ((g (gcd n d))
       (pn (/ (abs n) g))
       (pd (/ (abs d) g)))
    (if (< (* n d) 0)
        (cons (* -1 pn) pd)
        (cons pn pd))))

(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

;Exercise 2.2
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define make-point cons)

(define x-point car)
(define y-point cdr)

(define (midpoint-segment s)
  (make-point
   (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2.0)
   (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2.0)))

(define make-segment cons)

(define start-segment car)
(define end-segment cdr)

;Exercise 2.3
(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

(define (perimeter-rectangle r)
  (* (+ (width-rectangle r) (height-rectangle r)) 2))

(define make-rectangle cons)

(define (width-rectangle r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (height-rectangle r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

;Exercise 2.4
(define (lcons x y)
  (lambda (m) (m x y)))

(define (lcar m)
  (m (lambda (x y) x)))

(define (lcdr m)
  (m (lambda (x y) y)))

;Exercise 2.5
(define (power p n)
  (if
   (= n 1)
   p
   (* p (power p (- n 1)))))

(define (ncons x y)
  (* (power 2 x) (power 3 y)))

(define (ncar m)
  (if (= (remainder m 3) 0)
      (ncar (/ m 3))
      (/ (log m) (log 2))))

(define (ncdr m)
  (if (= (remainder m 2) 0)
      (ncdr (/ m 2))
      (/ (log m) (log 3))))

;Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-n-m n m) (lambda (f) (lambda (x) ((n (m f)) x))))

;Exercise 2.7
(define (mk-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define int1 (mk-interval 8 9))
(define int2 (mk-interval 6 8))

(define (add-interval x y)
  (mk-interval (+ (lower-bound x) (lower-bound y))
               (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (mk-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

;Exercise 2.8
(define (sub-interval x y)
  (mk-interval (- (lower-bound x) (lower-bound y))
               (- (upper-bound x) (upper-bound y))))

;Exervise 2.9
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

;Exercise 2.10
(define (div-interval x y)
  (if (= (width-interval y) 0)
      (error "interval spans 0")
      (mul-interval x (mk-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;Exercise 2.11
(define (mul-interval2 x y)
  (let ((lx (lower-bound x))
        (ly (lower-bound y))
        (ux (upper-bound x))
        (uy (upper-bound y)))
    (if (and (>= lx 0) (>= ly 0) (>= ux 0) (>= uy 0))
        (mk-interval (* lx ly) (* ux uy))
        (let ((p1 (* lx ly))
              (p2 (* lx uy))
              (p3 (* ux ly))
              (p4 (* ux uy)))
          (mk-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))))

;Exercise 2.12
(define (make-center-width c w)
  (mk-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define width width-interval)

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (let ((w (width i))
        (c (center i)))
    (* (/ w c) 100.0)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (mk-interval 1 1)))
    (div-interval
     one
     (add-interval (div-interval one r1)
                   (div-interval one r2)))))
