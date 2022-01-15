;;ex3.1
(define (make-accumulator acc)
  (lambda (adder) (set! acc (+ acc adder))))
;;ex3.2
(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?) count)
    (define (reset-count) (set! count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
	    ((eq? m 'reset-count) (reset-count))
	    (else (begin (set! count (+ count 1))
			 (f m)))))
    dispatch))
;;ex3.3
;;ex3.4
;;ex3.7
(define (make-account balance password)
  (let ((wrong-count 0))
    (define (withdraw amount)
      (if (>= balance  amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposite amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch p m)
      (if (eq? m 'check-pass)
	  (eq? p password)
	  (if (eq? p password)
	      (begin (set! wrong-count 0)
		     (cond ((eq? m 'withdraw) withdraw)
			   ((eq? m 'deposite) deposite)
			   (else (error "Unknown request: MAKE-ACCOUNT" m))))
	      (begin (set! wrong-count (+ wrong-count 1))
		     (if (>= wrong-count 7)
			 (constantly "Call the Cop")
			 (constantly "Incorrect password"))))))
    dispatch))

(define (make-joint account pass new-pass)
  (define (dispatch p m)
    (if (eq? p new-pass)
	(account pass m)
	(constantly "Incorrect password")))
  (if (account pass 'check-pass)
      dispatch
      (error "Incorrect password")))

;;ex3.5
(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (test-p p x1 x2 y1 y2)
    (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2))))
  (let ((rectarea (* (- x2 x1) (- y2 y1))))
    (* rectarea (monte-carlo trials (test-p p x1 x2 y1 y2)))))
(define (random-in-range low high)
  (use srfi-27)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed  trials))
	  ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (circle-p x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))
;;estimate pi
(/ (estimate-integral circle-p 2 8 4 10 1000) (* (square 3) 1.0))

;;ex3.6
(define random-init 0)
;;for test
(define (rand-update x)
  (+ x 1))
(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x)) x)
	    ((eq? m 'reset) (lambda (s) (set! x s)))
	    (else error "Unknown message.")))))
	 
;;ex3.8
(define f 
  (let ((s 0))
    (lambda (x)
      (if (= x s)
	(begin (set! s 1) 0)
	(begin (set! s 0) 1)))))
