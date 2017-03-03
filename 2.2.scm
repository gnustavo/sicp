; Section 2.2

(define nil (list))


; 2.17

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

;(last-pair (list 23 72 149 34))

; 2.18

(define (reverse list)
  (define (reverse-and-append list reversed)
    (if (null? list)
        reversed
        (reverse-and-append (cdr list)
                            (cons (car list) reversed))))
  (reverse-and-append list '()))

; (reverse (list 1 2 3 4))

; 2.19 

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

;(cc 100 uk-coins)

; 2.20

(define (same-parity head . tail)
  (cond ((null? tail) '())
        ((equal? (even? head) (even? (car tail))) (cons (car tail) (same-parity head (cdr tail))))
        (else (same-parity head (cdr tail)))))

; (same-parity 1 2 3 4 5 6 7)

; 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

; 2.22

; 2.23

(define (for-each proc list)
  (if (null? list)
      #t
      (begin (proc (car list))
            (for-each proc (cdr list)))))

;(for-each 
; (lambda (x) (newline) (display x))
; (list 57 321 88))

; 2.27

(define (deep-reverse list)
  (define (deep-reverse-and-append list reversed)
    (if (null? list)
        reversed
        (deep-reverse-and-append
          (cdr list)
          (cons 
            (if (pair? (car list))
                (deep-reverse-and-append (car list) '())
                (car list))
              reversed))))
  (deep-reverse-and-append list '()))

; 2.28

(define (fringe tree)
  (define (push-leaves tree leaves)
    (cond ((null? tree) leaves)
          ((pair? tree)
            (push-leaves (car tree) (push-leaves (cdr tree) leaves)))
          (else
            (cons tree leaves))))
  (push-leaves tree '()))

; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; 1

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define m (make-mobile (make-branch 2 (make-mobile (make-branch 1 3) (make-branch 2 3))) (make-branch 3 4)))

;; 2

(define (total-weight mobile)
  (define (branch-weight branch)
    (if (null? branch)
        0
        (let ((structure (branch-structure branch)))
           (if (pair? structure)
               (total-weight structure)
               structure))))
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

;; 3

(define (torque branch)
  (let ((structure (branch-structure branch)))
    (* (branch-length branch)
       (if (pair? structure)
           (total-weight structure)
           structure))))
(define (balanced? mobile)
 (= (torque (left-branch mobile)) (torque (right-branch mobile))))

;; 4

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(define m (make-mobile (make-branch 2 (make-mobile (make-branch 1 3) (make-branch 2 3))) (make-branch 3 4)))

; 2.30

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define t (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;(square-tree t)

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree tree))
        (else (map square-tree tree))))

;(square-tree t)

; 2.31

(define (tree-map proc tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(define (square-tree tree) 
  (tree-map (lambda (x) (* x x)) tree))

;(square-tree t)

; 2.32

(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map
                       (lambda (x) (append x (list (car s))))
                       rest)))))

;(subsets (list 1 2))

; 2.33

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; 2.34

(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

;(horner-eval 2 (list 1 3 0 5 0 1))

; 2.35

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; 2.37

(define (dot-product v w)
;  (accumulate + 0 (map * v w)))
  (if (null? v)
      0
      (+ (* (car v) (car w))
         (dot-product (cdr v) (cdr w)))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mrow) 
           (map (lambda (nrow)
                  (dot-product mrow nrow))
                cols))
              m)))

(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define vector (list 1 2 3 4))

;(dot-product vector vector)
;(matrix-*-vector matrix vector)
;(transpose matrix)
;(matrix-*-matrix matrix (transpose matrix))

; 2.38

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; it must be associative

; 2.39

(define (reverse sequence)
  (fold-right 
    (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left 
   (lambda (x y) (cons y x)) nil sequence))

;(reverse (list 1 2 3 4 5))
