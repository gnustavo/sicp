;; Section 2.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; 2.1

(define (make-rat numer denom)
  (let ((n (abs numer))
        (d (abs denom))
        (sign (if (equal? (< numer 0) (< denom 0)) 1.0 -1.0)))
    (cons (* sign n) d)))
 
(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

; 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
      (make-point (/ (+ (x-point start) (x-point end)) 2)
                  (/ (+ (y-point start) (y-point end)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(midpoint-segment (make-segment (make-point 1.0 2.0) (make-point 3.0 1.0)))

; 2.3

(define (segment-length seg)
    (let ((dx (- (x-point (end-segment seg)) (x-point (start-segment seg))))
          (dy (- (y-point (end-segment seg)) (y-point (start-segment seg)))))
        (sqrt (+ (* dx dx) (* dy dy)))))

(define (make-rectangle base height) ; base is a segment and height is a number
  (cons base height))

(define (rectanble-height rect)
  (cdr rect))

(define (rectangle-width rect)
  (segment-length (rectangle-base rect)))

(define (rectangle-perimeter rect)
  (let ((base-length (segment-length (rectangle-base rect))))
       (* 2 (+ base-length (rectanble-height rect)))))

(define (rectangle-area rect)
  (* (rectangle-width rect) (rectanble-height rect)))

(define (distance point-a point-b)
  (let ((dx (- (x-point point-b) (x-point point-a)))
        (dy (- (y-point point-b) (y-point point-a))))
      (sqrt (+ (* dx dx) (* dy dy)))))

(define (make-rectangle diagonal width height)
  (cons diagonal (cons width height)))

(define (rectangle-width rect)
  (car (cdr rect)))

(define (rectangle-height rect)
  (cdr (cdr rect)))

; 2.4

;(define (cdr z)
;  (z (lambda (p q) q)))

; 2.5

(define (power-of x n)
  (if (zero? n)
      1.0
      (* x (power-of x (- n 1)))))

(define (exponent-of x n)
  (if (zero? (mod x n))
    (+ 1 (exponent-of (div x n) n))
    0))

(define (arith-cons x y)
  (* (power-of 2.0 x) (power-of 3.0 y)))

(define (arith-car pair)
  (exponent-of pair 2))

(define (arith-cdr pair)
  (exponent-of pair 3))

(arith-cdr (arith-cons 5 4))

; 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))


(define (add a b)
  (define (effs n)
    (car (cdr (cdr (car (cdr (cdr n)))))))
  (define (add-effs l r)
    (if (pair? l)
        (add-effs (cdr l) (cons f r))
        r))
  (let ((afs (effs a))
        (bfs (effs b)))
      (cons lambda
            (cons (cons f null)
                  (cons (cons x null)
                        (add-effs afs bfs))))))

; 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound n)
  (car n))

(define (upper-bound n)
  (cdr n))

; 2.8

(define (sub-interval a b)
  (make-interval
    (- (lower-bound a) (upper-bound b))
    (- (upper-bound a) (lower-bound b))))

; 2.9

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

;a = [center(a) - width(a), center(b) + width(b)]
;lower(a) = center(a) - width(a)
;sum(a, b) = [lower(a) + lower(b), upper(a) + upper(b)]
;= [center(a) - width(a) + center(b) - width(b), center(a) + width(a) + center(b) + width(b)]
;= [(center(a) + center(b)) - (width(a) + width(b)), (center(a) + center(b)) + (width(a) + width(b))]
;= [center(a+b) - (width(a) + width(b)), center(a + b) + (width(a) + width(b))]

;width(a+b) = width(a) + width(b)

; 2.10

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0.0)
      (console-error "division by zero interval")
    (mul-interval x
                  (make-interval
                  (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 1.9 2.1) (make-interval -1.0 1.0))

; 2.11

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
      (cond
        ((> lx 0)
         (cond
           ((> ly 0) (make-interval (* lx ly) (* ux uy)))
           ((> uy 0) (make-interval (* ux ly) (* ux uy)))
           (else     (make-interval (* ux ly) (* lx uy)))))
        ((> ux 0)
         (cond
           ((> ly 0) (make-interval (* lx uy) (* ux uy)))
           ((> uy 0) (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))
           (else     (make-interval (* ux ly) (* lx ly)))))
        (else
         (cond
           ((> ly 0) (make-interval (* lx uy) (* ux ly)))
           ((> uy 0) (make-interval (* lx uy) (* lx ly)))
           (else     (make-interval (* ux uy) (* lx ly))))))))

; 2.12

(define (make-center-percent center percent)
  (make-interval (* center (- 1.0 percent)) (* center (+ 1.0 percent))))

(define (percent i)
  (/ (/ (width i) 2) (center i)))

; 2.13

;[lx, ux] * [ly, uy] =
;[lx * ly, ux * uy] =
;[(cx - tx) * (cy - ty), (cx + tx) * (cy + ty)] =
;[cx*cy - cx*ty - cy*tx + tx*ty, cx*cy + cx*ty + cy*tx + tx*ty] ~
;[cx*cy - cx*ty - cy*tx, cx*cy + cx*ty + cy*tx]

;ptx = tx/cx : tx = cx*ptx

;[cx*cy - (cx*cy*pty + cy*cx*ptx), cx*cy + (cx*cy*pty + cy*cx*ptx)] =
;[cx*cy - cx*cy(pty + ptx), cx*cy + cx*cy(ptx + pty)] =

;pt(x * y) ~ ptx + pty iff ptx, pty small

; 2.14

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))

; 2.14

; 2.15

; 2.16
