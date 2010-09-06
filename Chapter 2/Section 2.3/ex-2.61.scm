#lang scheme

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;Exercise 2.61
(define (adjoin-set x S)
  (cond ((null? S) (list x))
        ((< x (car S)) (cons x S))
        ((> x (car S)) (cons (car S) (adjoin-set x (cdr S))))
        ((= x (car S)) S)))

;;(adjoin-set 2 '(3 4 5))
;;(adjoin-set 2 '(1 3 4))
;;(adjoin-set 2 '(2 3 4))

;;The ordered representation will on average take less time, because
;;we do not always have to go over the entire list to adjoin the
;;element in the right place.

;;Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))
        ((= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1)
                          (cdr set2))))))