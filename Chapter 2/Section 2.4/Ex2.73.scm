#lang racket


(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

;; representing algebraic expressions
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

;; Because a primitive number does not have an operation tag, same for a symbol.
;; In fact, we could do the same, we would just have to redefine a number as
;; a (cons 'num 8) for example...

;; e.g. ((get 'deriv '+) '(1 2))

(define (addend s) (car s))

(define (augend s) (cadr s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (derive-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))


(define (multiplier p) (car p))

(define (multiplicand p) (cadr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (derive-product exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv  (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))

(put 'deriv '+ derive-sum)
(put 'deriv '* derive-product)

(deriv '(+ (* x y) (* 12 y)) 'y)

;; We'd just have to swap the put options to (put '+ 'deriv) ...
