#lang racket

;Exercise 2.56
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (mk-sum a1 a2) 
  (cond 
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

(define (mk-product a1 a2)
  (cond
    ((or (=number? a1 0) (=number? a2 0)) 0)
    ((=number? a1 1) a2)
    ((=number? a2 1) a1)
    ((and (number? a1) (number? a2)) (* a1 a2))
    (else (list '* a1 a2))))

(define (sum? x) 
  (and (pair? x) (eq? (car x) '+)))
(define addend cadr)
(define (augend s)
  (let ((r (cddr s)))
    (if (= (length r) 1)
        (car r)
        (cons '+ r))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define multiplier cadr)
(define (multiplicand s)
  (let ((r (cddr s)))
    (if (= (length r) 1)
        (car r)
        (cons '* r))))

;Exercise 2.56
(define (mk-exponentiation a1 a2)
  (cond 
    ((=number? a2 0) 1)
    ((=number? a2 1) a1)
    (else (list '^ a1 a2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))
(define base cadr)
(define exponent caddr)
         
(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (mk-sum (deriv (addend exp) var)
             (deriv (augend exp) var)))
    ((product? exp)
     (mk-sum (mk-product (multiplier exp)
                         (deriv (multiplicand exp) var))
             (mk-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
    ((exponentiation? exp)
     (mk-product (exponent exp) 
                 (mk-product (mk-exponentiation (base exp) (- (exponent exp) 1))
                             (deriv (base exp) var))))
    (else
     (error "unknown expression type --Deriv" exp))))

;Exercise 2.59
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;Exercise 2.60
(define element-of-set2? element-of-set?)
(define (adjoin-set2 x set)
  (cons x set))

(define (union-set2 set1 set2)
  (append set1 set2))

(define (intersection-set2 set1 set2)
  (filter (lambda (x) (element-of-set2? x set2)) set1))

;Exercise 2.61
(define (adjoin-set-o x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-o x (cdr set))))))

;Exercise 2.62
(define (intersection-set-o set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond
          ((= x1 x2) (cons x1 (intersection-set-o (cdr set1) (cdr set2))))
          ((< x1 x2) (intersection-set-o (cdr set1) set2))
          (else (intersection-set-o set1 (cdr set2)))))))

(define (union-set-o set1 set2)
  (cond 
    ((null? set1) set2)
    ((null? set2) set1)
    (else   
     (let ((x1 (car set1))
           (x2 (car set2)))
       (cond 
         ((= x1 x2) (cons x1 (union-set-o (cdr set1) (cdr set2))))
         ((< x1 x2) (cons x1 (union-set-o (cdr set1) set2)))
         (else (cons x2 (union-set-o set1 (cdr set2)))))))))
    
;Exercise 2.63
(define (entry tree) (car tree))
(define (right-branch tree) (caddr tree))
(define (left-branch tree) (cadr tree))
(define (make-tree entry left right)
  (list entry left right))
  
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree) remaining-elts))))

;Exercise 2.65
(define (union-set-bbt set1 set2)
  (list->tree (union-set-o (tree->list-1 set1) (tree->list-1 set2))))

(define (intersection-set-bbt set1 set2)
  (list->tree (intersection-set-o (tree->list-1 set1) (tree->list-1 set2))))
  
;Exercise 2.66
(define key-bbt car)
(define value-bbt cdr)

(define (lookup-bbt given-key set)
  (cond ((null? set) false)
        ((= given-key (key-bbt (entry set))) (value-bbt (entry set)))
        ((< given-key (key-bbt (entry set))) (lookup-bbt given-key (left-branch set)))
        (else (lookup-bbt given-key (right-branch set)))))

;Exercise 2.67
(define (mk-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))

(define (mk-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define left-h-branch car)
(define right-h-branch cadr)

(define (symbols  tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define sample-tree
  (mk-code-tree (mk-leaf 'A 4)
                (mk-code-tree
                 (mk-leaf 'B 2)
                 (mk-code-tree (mk-leaf 'D 1)
                               (mk-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-h-branch branch))
        ((= bit 1) (right-h-branch branch))
        (else (error "invalid encoding -- CHOOSE-BRANCH" bit))))

;Exercise 2.68
(define (symbol-in-tree? symbol tree)
  (element-of-set? symbol (symbols tree)))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         (if (eq? (symbol-leaf tree) symbol) '() (error "not symbol -- ENCODE-SYMBOL" symbol)))
        ((not (symbol-in-tree? symbol tree)) (error "not symbol -- ENCODE-SYMBOL" symbol))
        ((symbol-in-tree? symbol (left-h-branch tree)) (cons 0 (encode-symbol symbol (left-h-branch tree))))
        (else (cons 1 (encode-symbol symbol (right-h-branch tree))))))

(define (encode message tree)
  (if (null? message) 
      '()
      (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

;Exercise 2.69
(define (adjoin-h-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-h-set x (cdr set))))))

(define (mk-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-h-set (mk-leaf (car pair) (cadr pair))
                      (mk-leaf-set (cdr pairs))))))

(define (successive-merge pairs)
  (if (= (length pairs) 1)
      (car pairs)
      (successive-merge (adjoin-h-set (mk-code-tree (car pairs) (cadr pairs)) (cddr pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge (mk-leaf-set pairs)))

;Exercise 2.70
(define rock-tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
(define rock-tree2 (generate-huffman-tree '((A 1) (BOOM 1) (GET 1) (JOB 1) (NA 1) (SHA 1) (YIP 1) (WAH 1))))

(define rock-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

;Exercise 2.71
;most frequent symbol -> 1 bit
;least frequent symbol -> (n-1) bits