#lang scheme
;;Exercise 2.66
(define (lookup key records)
  (cond ((null? records) false)
        ((= key (entry records)) (record-value (entry records)))
        ((< key (entry records))
         (lookup key (left-branch records)))
        ((> key (entry records))
         (lookup key (right-branch records)))))

(define (make-record key value)
  (cons key value))