#lang scheme

;;Exercise 2.17
;;Iterative...
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;;Exercise 2.18
(define (remove-last l)
  (if (null? (cdr l))
      (cdr l)
      (cons (car l) (remove-last (cdr l)))))

(define (reverse l)
  (if (null? l)
      l
      (cons (last-pair l) (reverse (remove-last l)))))

;; EXERCISE 2.19 (define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;: (cc 100 us-coins)

(define (except-first-denomination coins) (cdr coins))
(define (first-denomination coins) (car coins))
(define (no-more? coins) (null? coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;No, the order does not matter, because
;;the algorithm checks every path possible,
;;removing the denominations which could "block"
;;further processing. eg. if we have (cc 25 (25 10 5))
;;the 25 branch immediately returns 1, but another
;;branch is also started without 25, allowing processing
;;of (10 5).

;;Exercise 2.20
(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (same-parity . a)
  (cond ((even? (car a)) (filter even? a))
        ((odd? (car a)) (filter odd? a))))

;;Exercise 2.21                                              
(define (square x) (* x x))
;;(define (square-list items)
;;  (if (null? items)
;;      '()
;;      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;;Exercise 2.22

;;To show why not, lets take a simple example.
;;Let's iterate over the list (list 1 2 3)
;;We get:
;;(iter (list 1 2 3) nil)
;;(iter (list 2 3) (list 1))
;;(iter (list 3) (list 4 1))
;;(iter () (list 9 4 1))
;;(list 9 4 1)
;;As can be seen, Louis is adding the the cdr of the list
;;to the car of the list.
;;If he interchanges the arguments, he gets:
;;(iter (list 1 2 3) nil)
;;(iter (list 2 3) (list nil 1))
;;(iter (list 3) (list nil 1 4))
;;(iter () (list nil 1 4 9))
;;(list nil 1 4 9)
;;Which doesn't work, because instead of consing single
;;elements, it has consed one big list to the next item
;;that is it results in:
;;(cons (cons (cons nil 1) 4) 9)
;;Instead of the (cons nil (cons 1 (cons 4 (cons 9))))
;;that louis wants. One solution would be to simply reverse
;;the list before or after iterating over it. 


;;Exercise 1.23
(define (my-for-each op items)
  (if (null? items)
      true
      (begin 
        (op (car items))
        (my-for-each op (cdr items)))))

(my-for-each (lambda (x) (newline)(display x)) (list 57 321 88))
