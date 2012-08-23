#lang racket/base
(require r5rs)
(require racket/dict)

(define (mk-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #false))
            #false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
  
(define operation-table (mk-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;Exercise 2.73
(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (mk-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (op-addend ops) (car ops))
(define (op-augend ops) (cadr ops))

(define (sum-deriv ops var)
  (mk-sum (deriv (op-addend ops) var)
          (deriv (op-augend ops) var)))
    
(define (mk-product a1 a2) 
  (cond ((=number? a1 0) 0)
        ((=number? a2 0) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))
(define (op-multiplicand ops) (car ops))
(define (op-multiplier ops) (cadr ops))

(define (product-deriv ops var)
  (mk-sum
   (mk-product (op-multiplier ops)
               (deriv (op-multiplicand ops) var))
   (mk-product (deriv (op-multiplier ops) var)
               (op-multiplicand ops))))
(define (mk-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list '** b e))))
(define (op-base ops) (car ops))
(define (op-exponent ops) (cadr ops))

(define (expo-deriv ops var)
  (mk-product
   (mk-product (op-exponent ops)
               (mk-exponentiation (op-base ops) (- (op-exponent ops) 1)))
   (deriv (op-base ops) var)))

(define (install-deriv-package)
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)
  (put 'deriv '** expo-deriv)
  'done)
(install-deriv-package)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else 
         ((get 'deriv (operator exp)) (operands exp)
                                      var))))

;Exercise 2.74
(define type-tag car)
(define contents cdr)
(define attach-tag cons)

(define (install-it-div-package)
  (define (get-record employed file)
    (if (dict-has-key? file employed)
        (tag (dict-ref file employed))
        #f))
  (define (get-salary record)
    (dict-ref record 'salary 0))
  (define (tag x) (attach-tag 'it-div x))
  (put 'get-record 'it-div get-record)
  (put 'get-salary 'it-div get-salary)
  'done)
(install-it-div-package)

(define it-div-file
  (attach-tag 'it-div #hash((luis . #hash((salary . 5001)))(zhen . #hash((salary . 5002))))))

(define (install-astro-div-package)
  (define (get-record employed file)
    (if (dict-has-key? file employed)
        (tag (dict-ref file employed))
        #f))
  (define (get-salary record)
    (dict-ref record 'salary 0))
  (define (tag x) (attach-tag 'astro-div x))
  (put 'get-record 'astro-div get-record)
  (put 'get-salary 'astro-div get-salary)
  'done)
(install-astro-div-package)

(define astro-div-file
  (attach-tag 'astro-div #hash((serena . #hash((salary . 4001)))(falocco . #hash((salary . 4002))))))

(define div-files (list it-div-file astro-div-file))

(define (get-record employed file)
  (let* ((tag (type-tag file))
         (proc (get 'get-record tag)))
    (if proc
        (apply proc (list employed (contents file)))
        (error "No method for -- GET-RECORD" tag))))

(define (get-salary record)
  (let* ((tag (type-tag record))
         (proc (get 'get-salary tag)))
    (if proc
        (apply proc (list (contents record)))
        (error "No method for -- GET-RECORD" tag))))

(define (find-employee-record employed files)
  (if (null? files)
      #f
      (let ((record (get-record employed (car files))))
        (if record
            record
            (find-employee-record employed (cdr files))))))
  
;Exercise 2.75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)