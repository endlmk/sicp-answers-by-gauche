;;ex2-1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n_r (/ n g))
	  (d_r (/ d g)))
      (if (< d_r 0)
	  (cons (* -1 n_r) (* -1 d_r))
	  (cons n_r d_r)))))

;;ex2-2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment s e)
  (cons s e))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (midpoint-segment seg)
  (let ((s (start-segment seg))
	(e (end-segment seg)))
    (make-point (/ (+ (x-point s) (x-point e)) 2.0) (/ (+ (y-point s) (y-point e)) 2.0))))

;;ex2-3
(define (make-rect s e)
  (cons s e))
(define (x-length r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (y-length r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))
(define (rect-perimeter r)
  (* 2 (+ (x-length r) (y-length r))))
(define (rect-area r)
  (* (x-length r) (y-length r)))
(define (make-rect2 c d)
  (cons c d))
(define (x-length r)
  (* (abs (x-point (cdr r))) 2))
(define (y-length r)
  (* (abs (y-point (cdr r))) 2))

;;ex2-4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

;;ex2-5
(define (cons1 a b)
  (* (expt 2 a) (expt 3 b)))
(define (car1 p)
  (define (car-for-expt p c)
    (cond ((= (mod p 2) 0) (car-for-expt (/ p 2) (+ c 1)))
	  (else c)))
  (car-for-expt p 0))
(define (cdr1 p)
  (define (cdr-for-expt p c)
    (cond ((= (mod p 3) 0) (cdr-for-expt (/ p 3) (+ c 1)))
	  (else c)))
  (cdr-for-expt p 0))

;;ex2-6
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))
(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;;ex2-7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

;;ex2-8
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
		 (- (upper-bound a) (lower-bound b))))

;;ex2-9
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(width (add-interval a b))
(/ (- (+ (upper-bound a) (upper-bound b)) (+ (lower-bound a) (lower-bound b))) 2)
(/ (+ (- (upper-bound a) (lower-bound a)) (- (upper-bound b) (lower-bound b))) 2)
(/ (+ (* (width a) 2) (* (width b) 2)) 2)
(width (sub-interval a b))
(/ (- (- (upper-bound a) (lower-bound b)) (- (lower-bound a) (upper-bound b))) 2)
(/ (+ (- (upper-bound b) (lower-bound b)) (- (upper-bound a) (lower-bound a))) 2)
(/ (+ (* (width a) 2) (* (width b) 2)))
;;mul-interval
;;(1 2) (3 4) -> (3 8) width:2.5
;;(1 2) (5 6) -> (5 12) width 3.5
;;div-interval
;;(1 2) (3 4) -> (1/4 2/3) width 5/24
;;(1 2) (5 6) -> (1/6 2/5) width 7/60

;;ex2-10
(define (div-interval x y)
  (if (> (* (lower-bound y) (upper-bound y)) 0)
      0
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
		      (/ 1.0 (lower-bound y))))))

;;ex2-11
(define (mul-interval x y)
  (cond ((and (>= (lower-bound x) 0) (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	((and (< (upper-bound x) 0) (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
	((and (< (lower-bound x) 0) (< (lower-bound y) 0)) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (>= (lower-bound x) 0) (< (lower-bound y) 0)) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
	((and (< (* (lower-bound x) (upper-bound x)) 0) (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
	((and (< (upper-bound x) 0) (< (* (lower-bound y) (upper-bound y)) 0)) (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (< (* (lower-bound x) (upper-bound x)) 0) (< (lower-bound y) 0)) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (>= (lower-bound x) 0) (< (* (lower-bound y) (upper-bound y)) 0)) (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	(else (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))) (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))))


