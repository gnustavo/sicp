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

