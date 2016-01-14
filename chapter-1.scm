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

;; 1.14 not code

;; 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

                                        ; a. log_3(12.15/0.1)

                                        ; b. growth in space ~ O(log(a))

;; 1.16

(define (expt b n)
  (expt-iter 1 b n))

(define (expt-iter a b n)
  (cond
   ((zero? n) a)
   ((even? n) (expt-iter a       (* b b) (/ n       2)))
   (else      (expt-iter (* a b) (* b b) (/ (- n 1) 2)))))

;; 1.17

(define (double i)
  (* i 2))

(define (halve i)
  (/ i 2))

(define (fast-mult a b)
  (cond
   ((zero? b) 0)
   ((= b 1)   a)
   ((even? b) (fast-mult (double a) (halve b)))
   (else      (+ a (fast-mult (double a) (halve (- b 1)))))))

;; 1.18

(define (mult-iter a b c)
  (cond
   ((zero? c) 0)
   ((= c 1)   (+ a b))
   ((even? c) (mult-iter a       (double b) (halve c)))
   (else      (mult-iter (+ a b) (double b) (halve (- c 1))))))

(define (mult b c)
  (mult-iter 0 b c))

;; 1.19

                                        ; T_pq
                                        ; a := bq + aq + ap
                                        ; b := bp + aq

                                        ; T_pq*T_pq
                                        ; a := (bp + aq)q + (bq + aq + ap)(q + p)
                                        ; b := (bp + aq)p + (bq + aq + ap)q

                                        ; a := b(2pq + qq) + a(2pq + qq) + a(pp + qq)
                                        ; b := b(pp + qq)  + a(2pq + qq)

                                        ; p' =  pp + qq
                                        ; q' = 2pq + qq


(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0)
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))   ; compute p’
                   (+ (* 2 p q) (* q q)) ; compute q’
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q)
                      (* a q)
                      (* a p))
                   (+ (* b p)
                      (* a q))
                   p
                   q
                   (- count 1)))))

;; 1.20 page 115
