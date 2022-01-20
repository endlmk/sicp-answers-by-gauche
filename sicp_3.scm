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

;;ex3.9
;;recursive
;;global-env -> [factorial]
;;               |
;;factorial E1->[n:6]
;;               |
;;factorial E2->[n:5]
;;               |
;;factorial E3->[n:4]
;;               |
;;factorial E4->[n:3]
;;               |
;;factorial E5->[n:2]
;;               |
;;factorial E6->[n:1]

;;iterative
;;gloval-env -> [factorial fact-iter]
;;               |
;;factorial E1->[n:6]
;;               |
;;fact-iter E2->[product:1 counter:1 max-count:6]
;;               |
;;fact-iter E3->[product:1 counter:2 max-count:6]
;;               |
;;fact-iter E4->[product:2 counter:3 max-count:6]
;;               |
;;fact-iter E5->[product:6 counter:4 max-count:6]
;;               |
;;fact-iter E6->[product:24 counter:5 max-count:6]
;;               |
;;fact-iter E7->[product:120 counter:6 max-count:6]
;;               |
;;fact-iter E8->[product:720 counter:7 max-count:6]

;;ex3.10
;;(define W1 (make-withdrawal 100))
;;global-env->[make-withdrawal:... W1:...         ]
;;                                  |           |
;;                                  (body, E1->[initial-amount:100])
;;                                    |          |
;;                                   (body, E2->[balance:100])
;;global-env->[make-withdrawal:... W1:...         ]
;;                                  |           |
;;                                 (body, E2->[balance:100])
;;(W1 50)
;;global-env->[make-withdrawal:... W1:...         ]
;;                                  |          |
;;                                 (body, E2->[balance:100])
;;                                             |
;;                                        E3->[amount:50])
;;global-env->[make-withdrawal:... W1:...         ]
;;                                  |          |
;;                                 (body, E2->[balance:50])
;;(define W2 (make-withdrawal 100))
;;global-env->[make-withdrawal:... W2:...         ]
;;                                  |           |
;;                                  (body, E4->[initial-amount:100])
;;                                    |          |
;;                                   (body, E5->[balance:100])
;;global-env->[make-withdrawal:... W2:...         ]
;;                                  |          |
;;                                 (body, E5->[balance:100])

;;ex3.11
;;(define acc (make-account 50))
;;global-env->[make-account:... acc:...                   ]
;;                               |                      |
;;                               (body(dispatch),E1)->[balance:50 withdraw:... deposite:... dispatch:...]
;;((acc 'deposite) 40)
;;global-env->[make-account:... acc:...                    ]
;;                               |                      |
;;                               (body(dispatch),E1)->[balance:50 withdraw:... deposite:... dispatch:...]
;;                                |
;;                          E2-> [m:'deposite]
;;global-env->[make-account:... acc:...]
;;                               |
;;                               (body(dispatch),E1)->[balance:50 withdraw:... deposite:... dispatch:...]
;;                                |
;;                               (body(deposite),E3)->[amount:40]
;;global-env->[make-account:... acc:...]
;;                               |
;;                               (body(dispatch),E1)->[balance:90 withdraw:... deposite:... dispatch:...]
;;((acc 'withdraw) 40)
;;global-env->[make-account:... acc:...]
;;                               |
;;                               (body(dispatch),E1)->[balance:50 withdraw:... deposite:... dispatch:...]
;;                                |
;;                           E4->[m:'withdraw]
;;global-env->[make-account:... acc:...]
;;                               |
;;                               (body(dispatch),E1)->[balance:50 withdraw:... deposite:... dispatch:...]
;;                                |
;;                               (body(deposite),E5)->[amount:60]
;;global-env->[make-account:... acc:...]
;;                               |
;;                               (body(dispatch),E1)->[balance:30 withdraw:... deposite:... dispatch:...]
;;accとacc2はそれぞれの束縛が別々の環境を指すことにより、別の局所状態を持つことになる。
;;共有される部分はない

;;ex3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y))
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(cdr x) ;;(b)
;;x->[・|・]->[・|/]
;;    |      |
;;    a      b
(define w (append! x y))
(cdr x) ;;(b c d
;;x->[・|・]->[・|・]->[・|・]->[・|/]
;;    |      |     |      |
;;    a      b     c      d

;;ex3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
(last-pair z) ;;無限ループする
;;    |-----------------
;;x->[・|・]->[・|・]->[・|・]」
;;    |      |     |     
;;    a      b     c     

;;ex3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
;;v->[・|・]->[・|・]->[・|・]->[・|/]
;;    |      |     |      |
;;    a      b     c      d
(define w (mystery v)) ;;(d c b a)
;;x->[・|・]->[・|・]->[・|・]->[・|/] y->[・|/]
;;    |      |     |      |
;;    a      b     c      d
;;x->[・|/] temp->[・|・]->[・|・]->[・|/]
;;    |           |     |      |
;;    a           b     c      d
;;x->[・|・]->[・|/] temp->[・|・]->[・|/]
;;    |      |          |      |    
;;    b      a          c      d
;;x->[・|・]->[・|・]->[・|/] temp->[・|/]
;;    |      |     |           |
;;    c      b     a           d
;;w->[・|・]->[・|・]->[・|・]->[・|/]
;;    |      |     |      |
;;    d      c     b      a

;;ex3.15
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x) (set-car! (car x) 'wow) x)
;;z1->[・|・]
;;     | |
;; x->[・|・]->[・|/]
;;     |      |
;;     a      b
(set-to-wow! z1) ;;((wow b) wow b)
;;z1->[・|・]
;;     | |
;; x->[・|・]->[・|/]
;;     |      |
;;     wow    b

;;z2->[・|・]
;;     |  \
;;    [・|・][・|・]
;;     | |  | |
;;     a b  a b
(set-to-wow! z2) ;;((wow b) a b)
;;z2->[・|・]
;;     |  \
;;    [・|・][・|・]
;;     | |  | |
;;    wow b  a b

;;ex3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
(define a (cons 0 1))
(define b (cons 2 3))
;;3
(cons a b)
;;  [・|・]
;;   |   \
;;  [・|・] [・|・]
;;   | |   | |
;;   0 1   2 3
;;4
(set-car! b a)
(cons a b)
;;  [・|・]
;; /  /
;; |[・|・]
;; \ |  \
;;  [・|・] 3
;;   | |
;;   1 2
;;7
(set-car! b a)
(set-cdr! b a)
(cons b b)
;;  [・|・]
;;   |/
;;  [・|・]
;;   |/  
;;  [・|・] 
;;   | |
;;   1 2
;;infinite
;;  |--------------|
;; [・|・]->[・|・]->[・|・]
;;  |     |      |
;;  1     2      3

;;ex3.17
(define (count-pairs1 x)
  (define counted ())
  (define (count-helper x)
    (if (not (pair? x))
	0
	(if (memq x counted)
	    0
	    (begin (set! counted (cons x counted))
	     	   (+ (count-helper (car x))
		      (count-helper (cdr x))
		      1)))))
  (count-helper x))

;;ex3.18
(define (is-infinite x)
  (define found ())
  (define (iter x)
    (if (not (pair? x))
	#f
	(begin (set! found (cons x found))
	       (print found)
	       (if (memq (cdr x) found)
		   #t
		   (iter (cdr x))))))
  (iter x))

;;ex3.19
(define (is-infinite-const x)
  (define (safe-cdr l)
    (if (pair? l)
	(cdr l)
	()))
  (define (iter tortoise hare)
    (cond ((not (pair? tortoise)) #f)
	  ((not (pair? hare)) #f)
	  ((eq? tortoise hare) #t)
	  (else (iter (safe-cdr tortoise) (safe-cdr (safe-cdr hare))))))
  (iter (safe-cdr x) (safe-cdr (safe-cdr x))))

;;ex3.20
(define x (cons 1 2))
;;grobal-env->[cons, car, cdr, set-car! set-cdr!]
;;             |
;;          x-(body, E1->[x:1, y:2])
(define z (cons x x))
;;grobal-env->[cons, car, cdr, set-car! set-cdr!]
;;             |                        |
;;          x-(body, E1->[x:1, y:2])  z-(body, E2->[x:x, y:x]
(set-car! (cdr z) 17)
;;grobal-env->[cons, car, cdr, set-car! set-cdr!]
;;             |                        |
;;          x-(body, E1->[x:1, y:17])  z-(body, E2->[x:x, y:x]
(car x)
;;y:17
