#lang scheme
;;Exercise 2.56
(require errortrace)

;;Split an expr on symb
(define (split-on symb expr)
  (define (iter r l)
    (cond ((eq? l '()) r)
          ((eq? symb (car l)) (cons r (cdr l)))
          (else (iter (append r (list (car l))) (cdr l)))))
  (iter '() expr))

;;Is symb in expr?
(define (in? symb expr)
  (cond ((eq? expr '()) false)
        ((eq? symb (car expr)) true)
        (else (in? symb (cdr expr)))))

;; representing algebraic expressions
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;Is expr a parenthized expression? eg. ((1 + 2))
(define (parenthized-expression? expr)
  (and (not (variable? expr)) 
       (not (number? expr))
       (= (length expr) 1)
       (or (sum? (car expr))
           (product? (car expr)))))
      
;;Most important thing of all:
(define (process-expr expr)
  (let ((expr (if (parenthized-expression? expr) (car expr) expr)))
    (cond ((if (= (length expr) 1) (variable? (car expr)) false) (car expr))
          ((if (= (length expr) 1) (number? (car expr)) false) (car expr))
          ((sum? expr) (make-sum (addend expr)
                                 (augend expr)))
          ((product? expr) (make-product (multiplier expr)
                                         (multiplicand expr))))))
;;'(+ x y)
(define (sum? x)
  (and (pair? x) (in? '+ x)))

(define (addend s) 
  (process-expr (car (split-on '+ s))))

(define (augend s)
  (process-expr (cdr (split-on '+ s))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;;'(* x y)
(define (product? x)
  (and (pair? x) (not (in? '+ x)) (in? '* x)))

(define (multiplier p)
  (process-expr (car (split-on '* p))))

(define (multiplicand p) 
  (process-expr (cdr (split-on '* p))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;;'(^ x y)
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation base exponent)
  (cond ((and (number? base) (number? exponent))
         (expt base exponent))
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '^ base exponent))))



(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((exponentiation? expr)
         (make-product 
           (make-product (exponent expr)
                         (make-exponentiation (base expr)
                                              (- (exponent expr) 1)))
           (deriv (base expr) var)))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        (else
         (error "unknown expression type -- DERIV" expr))))


;;(deriv '(+ x 3) 'x)
;;(deriv '(* x y) 'x)
;;(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
                                                 
