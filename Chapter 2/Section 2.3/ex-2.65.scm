#lang scheme

;;Binary Tree Representation
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (print-n-c n c)
  (display c)
  (if (> 0 n)
      '()
      (print-n-c (- n 1) c)))

(define (pprint-tree t)
  (define (rec t n)
    (if (null? t)
        '()
        (begin
          (newline)
          (display (print-n-c n " "))
          (display (entry t))
          (rec (left-branch t) (+ n 1)) 
          (rec (right-branch t) (+ n 1)))))
  (rec t 0))
;;general idea:
;;Walk the set2 til I find the right place for car set1, then add the car set1 to that place...

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (right-branch set1) (union-set (left-branch set1) (adjoin-set (entry set1) set2))))))

(define set1 (list->tree '(1 2 3 4 5)))
(define set2 (list->tree '(5 6 7 8 9)))

(pprint-tree (union-set set1 set2))