#lang racket

;; Exercise 2.27

(define tree1 (list (list 1 (list 3 4 5) 5 6 7) (list 8 9 10 11)))

(define tree2 (list (list 1 2) (list 3 4)))

(define (last-pair l)
 (cond ((pair? l)
        (if (null? (cdr l))
            (car l)
            (last-pair (cdr l))))
       (else l)))

(define (remove-last l)
  (if (null? (cdr l))
      '()
      (cons (car l) (remove-last (cdr l)))))

(define (deep-reverse l)
  (cond ((null? l) l)
        ((pair? l)
         (cons (deep-reverse (last-pair l)) (deep-reverse (remove-last l))))
        (else l)))
  
(deep-reverse tree2)


;; Exercise 2.28

(define (fringe tree)
  (define (iter tree)
    (cond ((not (pair? tree))
           (if (null? tree)
               '()
               (list tree)))
          (else (append (iter (car tree))
                      (iter (cdr tree))))))
  (iter tree))

(fringe tree1)


;; Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree) 
         (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (* tree tree))))

(square-tree tree1)

(define (square-tree2 tree)
 (map (lambda (sub-tree)
        (if (pair? tree)
            (square-tree sub-tree)
            (* sub-tree sub-tree)))
      tree))

(square-tree2 tree1)

;; Exercise 2.31 
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree3 tree)
  (tree-map (lambda (x) (* x x))
            tree))

(square-tree3 tree1)

;; Exercuse 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (display (cdr s)))))
        ;;(append rest (map (lambda (x) 
        ;;                    (cons (car s) x) )
        ;;                  rest)))))

(subsets (list 1 2 3))