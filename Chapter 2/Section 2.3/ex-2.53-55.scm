#lang scheme

;;Exercise 2.53

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list '(a b c))
;;> (a b c)
(list (list 'george))
;;> ((george))
(cdr '((x1 x2) (y1 y2)))
;;> (y1 y2)
(cadr '((x1 x2) (y1 y2)))
;;> y1
(pair? (car '(a short list)))
;;> #f
(memq 'red '((red shoes) (blue socks)))
;;> #f
(memq 'red '(red shoes blue socks))
;;> (red shoes blue socks) 

;;Exercise 2.54
(define (my-equal? a b)           
  (cond ((and (eq? a '()) (eq? b '())) true);;nil is a base case!
        ((and (symbol? a) (symbol? b)) 
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (my-equal? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))
        (else false)))

;;(newline)(display (my-equal? '(a b (c d)) '(a b (c d))))(newline)

;;Exercise 2.55
;;(car ''abracadabra)
;;(car (quote (quote abracadabra)))
;;(car (quote abracadabra))
;;quote
