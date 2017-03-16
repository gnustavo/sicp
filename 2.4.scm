;;; 2.4 Multiple Representations for Abstract Data

;;; 2.4.1 Representations for Complex Numbers

(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

;;; 2.4.2 Tagged data

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) 
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) 
     (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: 
               REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: 
               IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: 
               MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: 
               ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;; 2.4.3 Data-Directed Programming and Additivity

                                        ; 2.7.3
;; 1: Because numbers and variables are not operators

;; 2

(define (install-sum-package)
  (define (deriv sum var)
    (+ (deriv (addend sum) var) (deriv (augend sum) var)))
  (put 'deriv '(+) deriv)
  'done)

(define (install-product-package)
  (define (deriv product var)
    (+ (* (multiplier product) (deriv (multiplicand product) var))
       (* (deriv (multiplier product) var) (multiplicaed product))))
  (put 'deriv '(*) deriv)
  'done)

;; 3

(define (install-exponentiation-package)
  (define (deriv exp var)
    (* (exponent exp)
       (* (make-exponentiation (base exp) (- (exponent exp) 1))
          (deriv (base exp) var))))
  (put 'deriv '(^) deriv)
  'done)

;; 4

                                        ; 2.74

(define (get-record division employee-name)
  (apply-generic 'get-record division employe-name))

(define (get-salary employee)
  (apply-generic 'get-salary employee))

(define (find-employee-record employee-name divisions)
  (if (null? divisions)
      '()
      (let ((e (get-record (car divisions) employee-name)))
        (if (not (null? e))
            e
            (find-employee-record employee-name (cdr divisions))))))

                                        ; 2.75

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* m (cos a)))
          ((eq? op 'imag-part)
           (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op: 
            MAKE-FROM-MAG-ANG" op))))
  dispatch)

                                        ; 2.76

;; explicit dispatch: To add a new type one has to implement its specific methods and add a case in each generic procedure. To add a new operation one has to implement a specific method for each type and another generic procedure.

;; data-directed style: To add a new type one has to implement a new package. To add a new operation one has to add a method and a binding to each package and also to implement a new generic procedure.

;; message-passing style: To add a new type one has to implement a new message object. To add a new operation one has to add a method to each object and also to implement a new generic procedure.

;; frequent new types are better served by the two later styles.

;; frequent new operations are also better served by the two later styles.

