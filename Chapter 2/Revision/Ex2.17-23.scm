#lang racket

(define testlist (list 1 2 3 4 5 6 7 8))

;; Exercise 2.17

(define (last-pair l)
	(if (null? (cdr l))
		l
		(last-pair (cdr l))))


(last-pair testlist)

;; Exercise 2.18
(define (reverse l n)
	(if (null? l)
		n
		(reverse (cdr l) (cons (car l) n))))

(reverse testlist '())

;; Exercise 2.20
(define (same-parity x . n)
	(define (same-parity-num a b)
		(if (odd? a) 
			(odd? b) 
			(even? b)))
	(define (helper x n)
        (cond 
		((null? n) 
			'())
		((same-parity-num x (car n))
			(cons (car n) (helper x (cdr n))))
		(else 
			(helper x (cdr n)))))
	(helper x (cons x n)))

(same-parity 1 2 3 4 5 6 7)


;;Exercise 2.21
(define (square-list items)
        (if (null? items)
            '()
            (cons (* (car items) (car items)) 
                  (square-list (cdr items)))))

(square-list testlist)

(define (square-list2 items)
        (map (lambda (x) (* x x)) items))

(square-list2 testlist)

;; Exercise 2.23

(define (foreach procedure items)
        (if (null? items)
            '()
            (begin (procedure (car items))
                 (for-each procedure (cdr items)))))

(foreach (lambda (x) (newline) (display x)) (list 1 2 3 4 5))

