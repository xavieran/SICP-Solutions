 #lang scheme
;;Accumulate, &c.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;;Exercise 2.33
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;(horner-eval 2 (list 1 3 0 5 0 1))

;;(+ 1 (* 3 2) (* 5 (expt 2 3)) (expt 2 5))

;;Exercise 2.34
(define (count-leaves t)
  (accumulate (lambda (x y) (+ (if (not (pair? x)) 1 (count-leaves x)) y))
              0
              (map (lambda (x) x) t)))

;;count-leaves (list (list 1 2) (list 2 3 4) (list 2 (list 1))))

;;Exercise 2.36
(define (car-tree t)
  (map (lambda (l) (car l)) t))
(define (cdr-tree t)
  (map (lambda (l) (cdr l)) t))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init  (map (lambda (l) (car l)) seqs)) 
            (accumulate-n op init (map (lambda (l) (cdr l)) seqs)))))

;;(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;;(accumulate-n + 0 s)

;;Exercise 2.37

(define test-matrix '((2 2 2) (3 3 3) (4 4 4)))
(define test-vec '(1 2 3))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (element r c)
  (accumulate + 0 (accumulate-n * 1 (list r c))))

(define (matrix-*-vector m v)
  (map (lambda (r) (element r v)) m))

;;(matrix-*-vector '((1 2) (3 4)) '(1 2 3))

(define (dimensions m)
  (cons (length m) (length (car m))))

(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) '() mat))

(define (matrix-*-matrix m n)
  (if (not (= (cdr (dimensions m)) (car (dimensions n))))
      (error "Dimension mismatch")
      (let ((cols (transpose n)))
      (map (lambda (row) 
             (map (lambda (col) 
                    (element col row)) 
                  cols)) 
           m))))

;;(define the-3x2 '((1 4) (2 5) (3 6)))
;;(define the-2x4 '((1 2 3 4) (5 6 7 8)))
;;(define the-3x4 '((1 2 3 4) (5 6 7 8) (9 1 2 3)))

;;(dimensions the-3x2)
;;(dimensions the-2x4)
;;(dimensions the-3x4)

;;(matrix-*-matrix the-3x2 the-2x4)
;;(matrix-*-matrix the-3x4 the-2x4)
  
;;Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;;(fold-right / 1 (list 1 2 3))
;;(fold-left / 1 (list 1 2 3))
;;op must be commutative.
;;eg. 2+3=3+2, 2*3=3*2
;;However, / and - are not commutative, so
;;2-3!=3-2, 2/3!=3/2

;;Exercise 2.39
(define (right-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (left-reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

;;(right-reverse '(1 2 3))
;;(left-reverse '(1 2 3))

;;Exercise 2.40
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (filter (lambda (i) (not (eqv? i '())))
          (flatmap (lambda (i)
                   (map (lambda (j) (if (< j i) (list i j) '()))
                        (enumerate-interval 1 (- n 1))))
                   (enumerate-interval 1 n))))
;;(unique-pairs 6)

(define (in x l)
  (if (> (length (filter (lambda (i) (= i x)) l)) 0)
      true
      false))

(define primes (list 2 3 5 7 11 13 17 19 23))

(define (prime? n)
  (if (in n primes) 
      true 
      false))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;(prime-sum-pairs 6)

;;Exercise 2.41
(define (triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) 
                           (list i j k)) 
                         (enumerate-interval 1 n))) 
                  (enumerate-interval 1 n)))
            (enumerate-interval 1 n)))

;;(triples 3)

(define (in-order? cmp l)
  (cmp (car l) (cadr l) (caddr l)))

(define (gt? l)
  (in-order? > l))

(define (ordered-triples n)
  (filter gt? (triples n)))

(define (sum-to? triple s)
  (= (in-order? + triple) s))

(define (ordered-triples-which-sum-to-s n s)
  (filter (lambda (t) (sum-to? t s)) (ordered-triples n)))

;;(ordered-triples-which-sum-to-s 6 9)
 
;;Exercise 2.42
;;The queens will be shown by a list of x,y coordinates...
;;(list (list x1 y1) (list x2 y2) ... (list xn yn))

(define (make-posn row col)
  (list row col))

(define (posn-row posn)
  (car posn))

(define (posn-col posn)
  (cadr posn))

(define (posn-equal? p1 p2)
  (and (= (posn-col p1) (posn-col p2)) (= (posn-row p1) (posn-row p2))))

(define (in-los? p1 p2)
  (cond ((= (posn-row p1) (posn-row p2)) true)
        ((= (posn-col p1) (posn-col p2)) true)
        ((= (- (posn-row p1) (posn-row p2)) (- (posn-col p1) (posn-row p2))) true)
        (else false)))
  
(define (safe? k positions)
  (let* ((queen (car (filter (lambda (p) (= (posn-col p) k)) positions))) 
         (positions (filter (lambda (p) (posn-equal? queen p)) positions))
         (result (accumulate (lambda (p r) 
                               (if (not (in-los? queen p))
                                   (+ 1 r)
                                   (+ 0 r)))
                                   0
                   positions)))
    (display "queen:")(display queen)(newline)(display positions)(newline)(display result)
    (if (> result 0)
        true
        false)))

(define (adjoin-position new-row k roq)
  (append roq (list (make-posn new-row k))))

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
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size)) 

(queens 2)
(queens 8)


