#lang scheme

;;;SECTION 2.3.3

;; representing

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;(decode sample-message sample-tree)
;;(A D A B B C A)

;;Exercise 2.68
(define (element-of-set? symbol set)
  (cond ((null? set) false)
        ((eq? symbol (car set)) true)
        (else (element-of-set? symbol (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree - ENCODE-SYMBOL" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;;(decode (encode '(A D A B B C A) sample-tree) sample-tree)
;;(0 1 1 0 0 1 0 1 0 1 1 1 0)

;;Exercise 2.69
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge
       (adjoin-set (make-code-tree (car leaf-set)
                                   (cadr leaf-set)) 
                   (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define random (list (list 'A 4) (list 'B 2) (list 'D 1) (list 'C 1)))
(define mytree (generate-huffman-tree random))
;;Should be 13 like list above

;;Exercise 2.70
(define freq-pairs (list (list 'A 2) (list 'BOOM 1) (list 'GET 2) (list 'JOB 2) (list 'NA 16) (list 'SHA 3) (list 'YIP 9) (list 'WAH 1)))

(define message '(GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                  SHA BOOM))

(define rock50s-tree (generate-huffman-tree freq-pairs))

(define encoded-message (encode message rock50s-tree))
(define huffman-length (length encoded-message))

(define logbase2-of-8 3)
(define fixed-length (* (length message) logbase2-of-8))
;;fixed-length is 108 bits, whereas huffman-length is only 84 bits

;;Exercise 2.71
(define freq-pairs-5 (list (list '1 1) (list '2 2) (list '3 4) (list '4 8) (list '5 16)))
(define freq-pairs-tree-5 (generate-huffman-tree freq-pairs-5))

(define freq-pairs-10 (list (list '1 1) (list '2 2) (list '3 4) (list '4 8) (list '5 16) (list '6 32) (list '7 64) (list '8 128) (list '9 256) (list '10 512)))
(define freq-pairs-tree-10 (generate-huffman-tree freq-pairs-10))


;;It can be seen that the least frequent symbol requires n-1 bits, the most frequent symbol requires 1 bit.

;;Exercise 2.72
;;As we descend the tree we have to search the list of symbols at the node we ;;are on, this procedure is of the order of O(n). We will need to descend n ;;levels worst case, n searches n deep is O(n^2)