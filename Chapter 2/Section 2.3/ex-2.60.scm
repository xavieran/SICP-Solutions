#lang scheme

;;Unordered set representation
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (append set2 set1))


(define set1 '(a b c d e f))
(define set2 '(e f g h i j))

;;When duplicates are allowed,
;;element-of-set? is still O(n)
;;adjoin-set is O(1) from O(n)
;;intersection-set is still O(n^2)
;;union-set is O(n) from O(n^2)

;;The duplicate representation could be useful on a machine with limited processing power, but lots of memory