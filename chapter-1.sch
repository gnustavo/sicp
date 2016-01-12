;; Chapter 1

;; 1.1 not code

;; 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; 1.3

(define (takes-three-sum-squares-two-largest a b c)
  (cond
   ((and (< a b) (< a c))
    (+ (* b b) (* c c)))
   ((and (< b a) (< b c))
    (+ (* a a) (* c c)))
   (else
    (+ (* a a) (* b b)))))

;; 1.4 not code

;; 1.5 not code

;; 1.6 not code

;; 1.7 not code

;; 1.8

(define (cube-root x)
  (cube-root-iter 1 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve-cube-root guess x) x)))

; x/y^2 + 2y / 3

(define (improve-cube-root guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

;; 1.9 not code

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
                                        ; 2*n

(define (g n) (A 1 n))
                                        ; 2*g(n-1)...
                                        ; 2^n

(define (h n) (A 2 n))
                                        ; g(h(n-1))
                                        ; 2^h(n-1)

;; 1.11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-iter n)
  (if (< n 3)
      n
      (f-iter-loop 0 1 2 2 n)))

(define (f-iter-loop a b c i n)
  (if (= i n)
      c
      (f-iter-loop b c (+ c (* 2 b) (* 3 a)) (+ i 1) n)))

;; 1.12

(define (pascal lin col)
  (cond
   ((= col 1) 1)
   ((= lin col) 1)
   (else (+ (pascal (- lin 1) (- col 1))
            (pascal (- lin 1) col)))))

;; 1.13 not code

;; 1.14 page 104



