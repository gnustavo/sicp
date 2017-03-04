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

;; 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 206 40 6 4 2 0

(gcd 206 40)
  (= 40 0)
(gcd 40 (remainder 206 40))
  (= (remainder 206 40) 0)
  (= 6 0)
(gcd 6 (remainder 40 6))
  (= (remainder 40 6) 0)
  (= 4 0)
(gcd 4 (remainder 6 4))
  (= (remainder 6 4) 0)
  (= 2 0)
(gcd 2 (remainder 4 2))
  (= (remainder 4 2) 0)
  (= 0 0)
2

;; 1.21

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
; 199
(smallest-divisor 1999)
; 1999
(smallest-divisor 19999)
; 7

;; 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes begin end)
  (if (even? begin)
      (search-for-odd-primes (+ begin 1) end)
      (search-for-odd-primes begin       end)))

(define (search-for-odd-primes begin end)
  (cond
   ((< begin end)
    (timed-prime-test begin)
    (search-for-primes (+ begin 2) end))))

(search-for-primes 1000 1020)
(search-for-primes 10000 10039)
(search-for-primes 100000 100045)
(search-for-primes 1000000 1000039)
(search-for-primes 10000000 10000100)
(search-for-primes 10000000000 10000000100)
(search-for-primes 100000000000 100000000050)
; 1.15
(search-for-primes 1000000000000 1000000000100)
; 1.18
(search-for-primes 10000000000000 10000000000100)
; 3.66
(search-for-primes 100000000000000 100000000000100)
; 11.66

;; 1.23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (next test-divisor)))))

(search-for-primes 100000000000 100000000050)
; 1.15 -> 0.23
(search-for-primes 1000000000000 1000000000100)
; 1.18 -> 0.75
(search-for-primes 10000000000000 10000000000100)
; 3.66 -> 2.25
(search-for-primes 100000000000000 100000000000100)
; 11.66 -> 7.15

;; 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime)
                       start-time))))

(search-for-primes 100000000000 100000000050)
; 1.15 -> 0.23 -> 0.001
(search-for-primes 1000000000000 1000000000100)
; 1.18 -> 0.75 -> 0
(search-for-primes 10000000000000 10000000000100)
; 3.66 -> 2.25 -> 0
(search-for-primes 100000000000000 100000000000100)
; 11.66 -> 7.15 -> 0

;; 1.25 page 125

(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; Nope, because fast-expt would compute huge numbers while the original
; expmod keeps numbers small by taking their modulo (remainder) at each
; level.

;; 1.26 not code

;; 1.27 page 126

; 561, 1105, 1729, 2465, 2821, and 6601

(define (congruent? n)
  (define (try n a)
    (if (>= a n)
        true
        (and (= (expmod a n n) a)
             (try n (+ a 1)))))
  (try n 0))

(congruent? 561)
(congruent? 1105)
(congruent? 1729)
(congruent? 2465)
(congruent? 2821)
(congruent? 6601)

; all true!

;; 1.28

(define (expmod-nontrivial base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((sr (expmod-nontrivial base (/ exp 2) m))
                (rem (remainder (square sr) m)))
           (if (and (= rem 1) (not (= sr 1)) (not (= sr (- m 1))))
               0
               rem)))
        (else
         (remainder
          (* base (expmod-nontrivial base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-nontrivial a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n)
         (fast-prime-mr? n (- times 1)))
        (else false)))

(map (lambda (n) (fast-prime-mr? n 10))
     (list 2 3 4 5 6 7 8 9 10 11 12 13))

(define (carmichael)
  (list 561 1105 1729 2465 2821 6601))

(map (lambda (n) (fast-prime? n 10)) (carmichael))
(map (lambda (n) (fast-prime-mr? n 10)) (carmichael))

;; 1.29 page 134

(define (cube x) (* x x x))

(define (inc n) (+ n 1))
(define (inc2 n) (+ n 2))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
	(define (h)
  (/ (- b a) n))
	(define (y k)
	  (f (+ a (* k (h)))))
	(define (inc2 x) (+ x 2))
	(* (/ (h) 3)
	   (+
		(y 0)
		(* 4 (sum y 1 inc2 (- n 1)))
		(* 2 (sum y 2 inc2 (- n 2)))
		(y n))))

(cube 3)

(integral cube 0 1 1000)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity n)
  n)

(define (factorial n)
  (product identity 1 inc n))

(define (square n) (* n n))

(define (pi n)
  (define (term a) (square (/ a (- a 1))))
  (/
     (*
       4
       2
       (product term 4 inc2 (+ 4 (* 2 n))))
     (+ 5 (* 2 n))))

(factorial 5)

(pi 1000)

; 1.32
;; 1

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; 2

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(pi 1000)

; 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (filter a)
    (combiner
      (term a)
      (accumulate combiner null-value term (next a) next b)
    null-value)))

;(filtered-accumulate prime? + 0 square a inc b)

; 1.34

(define (f g) (g 2))

(square 2)
(f square)

; 1.35

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
;    (display guess)
;    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; 1.36

(fixed-point
  (lambda (x) (/ (+ x (/ (log 1000.0) (log x))) 2))
  10.0)

; 1.37

;; 1

(define (cont-frac n d k)
  (define (cont-frac-i i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (cont-frac-i (+ i 1))))))
  (cont-frac-i 1))

; 1/G = 0.6180344478216819

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

; 0.6180555555555556

;; 2  FIXME

; 1.38

(define (d-euler i)
  (if (= 2 (mod i 3))
      (* (/ (+ i 1) 3) 2)
      1))
(cont-frac (lambda (i) 1.0)
           d-euler
           100)

; 1.39

(define (tan-cf x k)
  (define (n i) (- 0 (* x x)))
  (define (d i) (+ 1 (* 2 i)))
  (/ x (+ 1 (cont-frac n d k))))

(define (PI) 3.141592654)

(tan-cf (* (/ (PI) 2) 0.9) 100)

;;; 1.3.4

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))
 
(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
   1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

; 1.40

(define (cubic a b c)
  (lambda (x) (+ c (* x (+ b (* x (+ a x)))))))
              

(newtons-method (cubic 1 1 1) 1)

; 1.41

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc)   6)

; 1.43

(define (repeated f n)
  (cond
    ((zero? n) identity)
    ((even? n) (repeated (compose f f) (/ n 2)))
    (else (compose f (repeated f (- n 1))))))
 
((repeated square 2) 5)

; 1.44

(define (smooth f)
  (let ((dx 0.001))
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
         3))))

;((repeated smooth n) f)

; 1.45

(define (nth-power x n)
  (cond
    ((zero? n) 1)
    ((even? n) (let ((p (nth-power x (/ n 2)))) (* p p)))
    (else      (* x (nth-power x (- n 1)))))) 

(define (nth-root x n)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (nth-power y (- n 1)))))
               1.0))

(nth-root 100 8)

; 1.46

(define (iterative-improvement good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (lambda (guess) (try guess)))


(define (sqrt x)
  ((iterative-improvement
     (lambda (guess) (< (abs (- (square guess) x)) 0.001))
     (lambda (guess) (average guess (/ x guess))))
    1.0))
 
(sqrt 4.0)


;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2))
;       tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess));

(define (fixed-point f first-guess)
  ((iterative-improvement
     (lambda (guess) (< (abs (- guess (f guess))) 0.001))
     (lambda (guess) (f guess)))
   first-guess))

(fixed-point cos 3.0)
