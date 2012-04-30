#lang racket/base
(require racket/mpair)

(define (mk-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record (mcdr record) #false))
            #false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
  
(define operation-table (mk-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (type-tag x)
  (cond ((number? x) 'scheme-number)
        (else (car x))))
(define (contents x)
  (cond ((number? x) x)
        (else (cdr x))))
(define attach-tag cons)

(define (square n) (* n n))

;Exercise 2.77
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let* ((type1 (car type-tags))
                   (type2 (cadr type-tags))
                   (a1 (car args))
                   (a2 (cadr args))
                   (t1->t2 (get-coercion type1 type2))
                   (t2->t1 (get-coercion type2 type1)))
              (cond ((eq? type1 type2)
                     (error "No method for these types" (list op type-tags)))
                    (t1->t2
                     (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                     (apply-generic op a1 (t2->t1 a2)))
                    (else
                     (error "No method for these types" (list op type-tags)))))
            (error "No method for these types" (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'level 'real 0)
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)
(define (make-real x)
  ((get 'make 'real) x))
(define (real-level) (get 'level 'real))
(install-real-package)
  
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (tag x) (attach-tag 'rational x))
  (put 'raise '(rational) 
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'level 'rational (- (real-level) 1))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (rational-level) (get 'level 'rational))
(install-rational-package)

(define (install-scheme-number-package)
  (define (=zero? x) (eq? x 0))
  (define (tag x) x)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-number scheme-number) eq?)
  (put '=zero? '(scheme-number) =zero?)
  (put 'raise '(scheme-number) 
       (lambda (n) (make-rational n 1)))
  (put 'level 'scheme-number (- (rational-level) 1))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(install-scheme-number-package)

(define (install-rectangular-package)
  (define real-part car)
  (define imag-part cdr)
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (make-from-real-imag x y) (cons x y))
  (define (tag z) (attach-tag 'rectangular z))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)
(install-rectangular-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (equ? z1 z2)
    (and (eq? (real-part z1) (real-part z2)) (eq? (imag-part z1) (imag-part z2))))
  (define (=zero? z1)
    (and (eq? (real-part z1) 0) (eq? (imag-part z1) 0)))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define z (make-complex-from-real-imag 3 4))
(define v (make-complex-from-real-imag 3 4))
(define u (make-complex-from-real-imag 4 3))

;Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))

;Exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;Exercise 2.81
(define coercion-table (mk-table))
(define get-coercion (operation-table 'lookup-proc))
(define put-coercion (operation-table 'insert-proc!))

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

;Exercise 2.83
(define (raise x) (apply-generic 'raise x))

;Exercise 2.84
(define (level x) (get 'level (type-tag x)))
