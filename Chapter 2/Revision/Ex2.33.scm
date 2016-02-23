#lang racket

(define (accumulate op null sequence)
  (cond ((pair? sequence)
         (op (car sequence) (accumulate op null (cdr sequence))))
        (else null)))

(accumulate + 0 '(1 2 3 4))

(define (mapa p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(mapa (lambda (x) (* x x)) '(1 2 3 4))

(define (appenda seq1 seq2)
  (accumulate cons seq2 seq1))

(appenda '(1 2 3) '(4 5 6))

(define (lengtha sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(lengtha '(1 2 3 4 5 6))

;; Exercise 2.36 

(define seq '((1 2 3)
  (1 2 3)
  (1 2 3)))

(define (element-at n seq)
  (define (h c n seq)
    (if (= c n)
        (car seq)
        (h (+ c 1) n (cdr seq))))
  (h 0 n seq))

(define (accumulate-n op init seq)
  (let ((len (length (car seq))))
  (define (h c seq)
    (cond ((= c len)
           '())
          (else (cons 
                 (accumulate op init (map (lambda (x) (element-at c x))
                                          seq))
                 (h (+ c 1) seq)))))
  (h 0 seq)))

(accumulate-n * 1 seq)

(define (Accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(Accumulate-n * 1 seq)

;; Exercise 2.37

(define (dot-product v w)
  (let ((seqs (list v w)))
    (accumulate + 0 
                (accumulate-n * 1 seqs))))

(dot-product '(1 2 3) '(1 2 3))

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

(matrix-*-vector '((1 0 0)
                   (0 2 0)
                   (0 0 1))
                 '(2 3 4))

(define (matrix-*-matrix m1 m2)
  (map (lambda (m2i) (matrix-*-vector m1 m2i)) m2))

(matrix-*-matrix '((1 0 0)
                   (0 2 0) 
                   (0 0 3))
                 '((0 1 0)
                   (0 0 1) 
                   (1 0 0)))

(define mat '((1 2 3)
               (4 5 6)
               (7 8 9)))

(define matt '((1 4 7)
               (2 5 8)
               (3 6 9)))

(define (transpose m)
  (accumulate-n (lambda (x y) (cons x y)) '() m))

(transpose mat)

;; Op needs to satisfy commutative property... (op A B) = (op B A)

;; Ex 2.39
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reversel '(1 2 3 4 5))

(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x)))
                '() sequence))

(reverser '(1 2 3 4 5))

;; Exercise 2.40
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumi i j)
  (define (h c)
    (if (> c j)
        '()
        (cons c (h (+ 1 c)))))
  (h i))

(enumi 10 20)

;;flatmap -> for each element in enumi
;;           apply procedure to element
;;           the result of procedure must be a list
;;           append the result to the larger list

(define (generate-pairs n)
  (flatmap 
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumi 1 (- i 1))))
   (enumi 1 n)))

(generate-pairs 5)
  
(flatmap (lambda (i) (list i (+ i 1))) (enumi 1 5))

(define (unique-pairs n)
  (flatmap 
   (lambda (i) 
     (map (lambda (j) (list i j))
          (enumi 1 (- i 1))))
   (enumi 1 n)))

(unique-pairs 5)

(display 'Exercise2.41)
(newline)
;; Exercise 2.41
(define (triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
          (map (lambda (k) (list i j k))
               (enumi 1 n)))
          (enumi 1 n)))
   (enumi 1 n)))

(define (in-order? t)
  (> (car t) (cadr t) (caddr t)))
        
(define (ordered-triples ts)
  (filter in-order? ts))

(define (sum-to-less? n t)
  (< (+ (car t) (cadr t) (caddr t)) n))

(define (triples-that-sum-to-less-than s n)
  (filter (lambda (x) (sum-to-less? s x)) (ordered-triples (triples n))))

(triples-that-sum-to-less-than 10 20)

;; Exercise 2.42

(define safe-pos '(1 3 4 2))
(define unsafe-pos '(1 3 2 4))



(define (safe? k positions)
  (define (h new-pos positions r)
    (cond ((null? positions) #true) ;; No other queens
          ((= new-pos (car positions)) #false)
          ((= (abs (- new-pos (car positions))) (abs (- 1 r))) #false)
          (else (h new-pos (cdr positions) (+ 1 r)))))
  (h (car positions) (cdr positions) 2))

(safe? 1 safe-pos)
(safe? 1 unsafe-pos)

(define (adjoin-position nr k roq)
  (cons nr roq))

(define empty-board '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumi 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
