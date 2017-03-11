;; Section 2.3

                                        ; 2.53

                                        ;(a b c)
                                        ;((george))
                                        ;((y1 y2))
                                        ;(y1 y2)
                                        ;#f
                                        ;#f
                                        ;(red shoes blue socks)

                                        ; 2.54

(define (equal? a b)
  (cond ((pair? a)
         (if (pair? b)
             (and (equal? (car a) (car b))
                  (equal? (cdr a) (cdr b)))
             #f))
        ((null? a)
         (null? b))
        (else
         (and (not (or (pair? b) (null? b))) (eq? a b)))))


                                        ; 2.55

(car ''abracadabra)

;; 2.3.2

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((pair? exp)
         (if (null? (cdr exp))
             (deriv (car exp) var)
             (error "DERIV: invalid sub-expression: " exp)))
        (else (error "DERIV: unknown expression type: " exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

                                        ; 2.56

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2))
         (exp e1 e2))
        (else (list '** e1 e2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv exp var)
  (display exp)(newline)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (- (exponent exp) 1))
           (deriv (base exp) var))))
        (else (error "unknown expression type: DERIV" exp))))

                                        ;(deriv (make-exponentiation 'x 3) 'x)

                                        ; 2.57

(define (make-sum-list addends)
  (define (summ sumnum sumterms terms)
                                        ;(format #t "sum-terms ~s ~s ~s ~%" sumnum sumterms terms)
    (cond ((null? terms)
           (cond ((zero? sumnum) sumterms)
                 (else (cons sumnum sumterms))))
          ((number? (car terms))
           (summ (+ sumnum (car terms)) sumterms (cdr terms)))
          (else
           (summ sumnum (cons (car terms) sumterms) (cdr terms)))))
  (let ((summed (summ 0 '() addends)))
                                        ;(format #t "body: ~s ~%" summed-terms)
    (cond ((null? summed) 0)
          ((pair? summed)
           (if (null? (cdr summed))
               (car summed)
               (cons '+ summed)))
          (else                 summed))))

(define (make-sum . addends) (make-sum-list addends))

                                        ;(make-sum 0)

(define (augend s) (make-sum-list (cddr s)))

(define (make-product-list multiplicands)
  (define (multiply prodnum prodterms terms)
                                        ;(format #t "multiply ~s ~s ~s ~%" prodnum prodterms terms)
    (cond ((null? terms)
           (cond ((zero? prodnum) 0)
                 ((= 1 prodnum) prodterms)
                 (else (cons prodnum prodterms))))
          ((number? (car terms))
           (multiply (* prodnum (car terms)) prodterms (cdr terms)))
          (else
           (multiply prodnum (cons (car terms) prodterms) (cdr terms)))))
  (let ((multiplied (multiply 1 '() multiplicands)))
                                        ;(format #t "body: ~s ~%" multiplied)
    (cond ((null? multiplied) 0)
          ((pair? multiplied)
           (if (null? (cdr multiplied))
               (car multiplied)
               (cons '* multiplied)))
          (else                 multiplied))))

(define (make-product . multiplicands) (make-product-list multiplicands))

(define (multiplicand p) (make-product-list (cddr p)))

                                        ;(make-product 1 2 'z 'x)

                                        ;(deriv '(* x y (+ x 3)) 'x)

                                        ; 2.58

;; 1

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list m1 '* m2))))

                                        ;(deriv '(x + (3 * (x + (y + 2)))) 'x)

;; 2

(define (infix2prefix exp)
  (define (prefix result product terms)
                                        ;(format #t "prefix: ~s ~s ~s ~%" result product terms)
    (cond ((null? terms)
                                        ;(format #t "null ~%")
           (if (null? product)
               result
               (list '+ product result)))
          ((not (pair? terms))
                                        ;(format #t "not pair ~%")
           (if (null? product)
               (if (null? result)
                   (list terms)
                   (list '+ (list terms) result))
               (if (null? result)
                   (list '+ (list terms) product)
                   (list '+ (list terms) (list '+ product result)))))
                                        ; assume a list in the form (term op term op term op ...)
          ((null? (cdr terms))
                                        ;(format #t "null cdr ~%")
           (if (null? product)
               (if (null? result)
                   (list (car terms))
                   (list '+ (car terms) result))
               (if (null? result)
                   (list '+ (car terms) product)
                   (list '+ (car terms) (list '+ product result)))))
          ((eq? (cadr terms) '*)
                                        ;(format #t "* ~%")
           (if (null? product)
               (prefix result (list (car terms))            (cddr terms))
               (prefix result (list '* (car terms) product) (cddr terms))))
          ((eq? (cadr terms) '+)
                                        ;(format #t "+ ~%")
           (if (null? product)
               (if (null? result)
                   (prefix (list (car terms))                             '() (cddr terms)))
               (prefix (list '+ (car terms) result)                   '() (cddr terms)))
           (if (null? result)
               (prefix (list '+ (car terms) product)                  '() (cddr terms))
               (prefix (list '+ (car terms) (list '+ product result)) '() (cddr terms))))
          (else (error "invalid expression: ~s" terms))))
  (prefix '() '() exp))

                                        ;(infix2prefix '(x + 3 * (x + y + 2)))

                                        ;(infix2prefix '(1 + 2))


(define (find-eq? elem list)
  (cond ((null? list) '())
        ((eq? elem (car list)) list)
        (else (find-eq? elem (cdr list)))))

(define (has-eq? elem list)
  (not (null? (find-eq? elem list))))

(define (cut-at-eq? elem list)
  (cond ((or (null? list) (eq? elem (car list)))
         '())
        (else
         (cons (car list)
               (cut-at-eq? elem (cdr list))))))

(define (listify x)
  (if (pair? x)
      x
      (list x)))

(define (sum? x)
  (has-eq? '+ x))

(define (addend s) (cut-at-eq? '+ s))
(define (augend s) (cdr (find-eq? '+ s)))

(define (product? x)
  (and (has-eq? '* x) (not (sum? x))))

(define (multiplier p) (cut-at-eq? '* p))

(define (multiplicand p) (cdr (find-eq? '* p)))

(define (make-sum first . rest)
  (cond ((null? rest) (listify first))
        (else (append (listify first)
                      (list '+)
                      (apply make-sum rest)))))

(define (make-product first . rest)
  (define (parenthesize-if-needed exp)
    (if (has-eq? '+ exp) (list exp) exp))
  (cond ((null? rest) (parenthesize-if-needed (listify first)))
        (else (append (parenthesize-if-needed (listify first))
                      (list '*)
                      (apply make-product rest)))))


;(deriv (make-sum 'x (make-product 3 (make-sum 'x 'y 2))) 'x)

;; 2.3.3 Sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

                                        ; 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1)
               (union-set (cdr set1) set2)))))

                                        ; 2.60

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;; Sets as ordered lists

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
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

                                        ; 2.61

(define (adjoin-set x set)
  (cond ((null? set) '())
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

                                        ; 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set 
                            (cdr set1)
                            (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 (else
                  (cons x2 (union-set set1 (cdr set2)))))))))

;; Sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

                                        ; 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;; 1 No. The first is left-entry-right and the second is entry-right-left order.

;; 1 3 5 7 9 11
;; 1 3 5 7 9 11
;; 1 3 5 7 9 11

;; 7 9 11 3 5 1
;; 3 7 9 11 5 1
;; 5 9 11 7 3 1

;; 2 Yes

                                        ; 2.64

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

;(7
; (3
;  (1
;   ()
;   ())
;  (3
;   ()
;   ()))
; (9
;  (7
;   ()
;   ())
;  (11
;   ()
;   ())))

;; It's O(n)

                                        ; 2.65
(define (union-set set1 set2)
  (list->tree
   (union-set (tree->list1 set1)
              (tree->list1 set2))))

(define (intersection-set set1 set2)
  (list->tree
   (intersection-set (tree->list1 set1)
                     (tree->list1 set2))))

;; Sets and information retrieval

                                        ; 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set) false)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else
         (lookup given-key (right-branch set-of-records)))))

;; 2.3.4 Example: Huffman Encoding Trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

                                        ; 2.67

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;(decode sample-message sample-tree)
;;(a d a b b c a)

                                        ; 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))

(define (symbol-of-set? x set)
  (cond ((null? set) false)
        (else
         (memq x (symbols set)))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (cond ((symbol-of-set? symbol (left-branch tree))
             (cons 0 (encode-symbol symbol (left-branch tree))))
            ((symbol-of-set? symbol (right-branch tree))
             (cons 1 (encode-symbol symbol (right-branch tree))))
            (else (error "bad symbol:
                          ENCODE-SYMBOL" symbol)))))

                                        ; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        (else
         (successive-merge
          (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                      (cddr leaf-set))))))


                                        ; 2.70

;; 84 bits

;; 36 * 3 = 108 bits!

                                        ; 2.71

;; 1 bit
;; n-1 bits

                                        ; 2.72

;; Supposing the tree is relatively balanced, encode-symbol is called
;; recursivelly log(n) times for each symbol. At each node in the tree above
;; the leaves it must search the symbol in the symbols list of one or two
;; branches. The length of the symbols list of a branch is the number of
;; nodes in the branch. So, at each level it must search on average about
;; half the number of nodes below that level. Or O(sum(i=0; log(n);
;; 2^(i-1))). I guess it's about O(n).

;; most frequent: O(n), because it only searches on the symbols list of the
;; root branches.

;; least frequent: O(n*log(n)), because it searches on the symbols list of
;; all branches.
