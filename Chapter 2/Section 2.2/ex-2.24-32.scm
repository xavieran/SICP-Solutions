#lang scheme

;;Exercise 2.24
;;(1 (2 (3 4)))
;;(display (list 1 (list 2 (list 3 4))))

;;Exercise 2.25
;;Note, when I did these on paper, I got the order the wrong way around
;;(define a (list 1 3 (list 5 7) 9))
;;(display (car (cdr (car (cdr (cdr a))))))
;;(define b (list (list 7)))
;;(display (car (car b)))
;;(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;;(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))))))

;;Exercise 2.26
;;(display (append x y))
;;->(1 2 3 4 5 6)
;;(display (cons x y)
;;->((1 2 3) (4 5 6))
;;(display (list x y))
;;->((1 2 3) (4 5 6))

;;Exercise 2.27
(define (deep-reverse l)
  (define (iter l r)
    (cond ((null? l) r)
          ((pair? (car l)) (iter (cdr l) (cons (deep-reverse (car l)) r)))
          (else (iter (cdr l) (cons (car l) r)))))
  (iter l '()))

;;(display (deep-reverse (list (list 1 2 3) (list 4 5 6))))

;:Exercise 2.28
(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? l)) (cons l '()))
        (else (append (fringe (car l)) (fringe (cdr l))))))

;;(display (fringe (list (list 1 (list 2 3) 4) (list 5 6))))

;;Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

;;Part A
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;;Part B
(define my-mobile (make-mobile (make-branch 1 4)
                               (make-branch 1 (make-mobile (make-branch 1 2)
                                                           (make-branch 1 2)))))
                                                          

(define (branch-weight branch)
  (if (number? (branch-structure branch))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))))

;;(display (total-weight my-mobile))

;;Part C                  
(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (branch-balanced? branch)
  (if (number? (branch-structure branch))
      true
      (balanced? (branch-structure branch))))

(define (balanced? mobile)
  (and (= (torque (right-branch mobile)) (torque (left-branch mobile)))
       (and (branch-balanced? (right-branch mobile)) 
            (branch-balanced? (left-branch mobile)))))
  
;;(display (balanced? my-mobile))  

;;Part D
;;We only need to change the selectors
;;(define (left-branch mobile)
;;  (car mobile))
;;(define (right-branch mobile)
;;  (cdr mobile))
;;(define (branch-length branch)
;;  (car branch))
;;(define (branch-structure branch)
;;  (cdr branch))


;;Exercise 2.30
(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

;;display (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

;;Exercise 2.31
(define (tree-map op tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (op tree))
        (else (cons (tree-map op (car tree)) (tree-map op (cdr tree))))))

;;(display (tree-map square (list 1 (list 2 (list 3 4) 5) (list 6 7))))

;;Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;(display (subsets (list 1 2 3)))

