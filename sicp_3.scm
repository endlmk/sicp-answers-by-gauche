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

;;ex3.21
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons () ()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONTcalled with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item ())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else (set-front-ptr! queue (cdr (front-ptr queue)))
	      queue)))
;;make-queueは開始ポインタと終了ポインタのペアである
;;開始ポインタは内部のリストの開始を指すため、内部のリストを表示している
;;終了ポインタは内部リストの末尾のペアを指している
;;これらのconsのため、先頭がリスト、次が末尾の要素となるリストで表示されている
(define (print-queue queue)
  (front-ptr queue))
;;ex3.22
(define (make-queue)
  (let ((front-ptr ())
	(rear-ptr ()))
    (define (insert! item)
      (let ((new-pair (cons item ())))
	(cond ((null? front-ptr)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    (define (delete!)
      (if (null? front-ptr)
	  (error "DELETE! called with empty queue")
	  (set! front-ptr (cdr front-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'front-queue) (car front-ptr))
	    ((eq? m 'insert-queue!) insert!)
	    ((eq? m 'delete-queue!) (delete!))
	    ((eq? m 'print-queue) front-ptr)
	    (else error "Undefined operation: QUEUE" m)))
    dispatch))
(define (front-queue queue) (queue 'front-queue))
(define (insert-queue! queue item) ((queue 'insert-queue!) item) queue)
(define (delete-queue! queue) (queue 'delete-queue!) queue)
(define (print-queue queue) (queue 'print-queue))

;;ex3.23
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-bidirect-item value) (cons value (cons () ())))
(define (next-ptr bidirect) (cadr bidirect))
(define (prev-ptr bidirect) (cddr bidirect))
(define (get-value bidirect) (car bidirect))
(define (set-next-ptr! bidirect item) (set-car! (cdr bidirect) item))
(define (set-prev-ptr! bidirect item) (set-cdr! (cdr bidirect) item))
(define (make-deque) (cons () ()))
(define (front-queue deque)
  (if (empty-deeue? deque)
      (error "FRONT called with an empty deque" deque)
      (get-value (front-ptr queue))))
(define (front-insert-queue! deque value)
  (let ((new-item (make-bidirect-item value)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-item)
	   (set-rear-ptr! deque new-item)
	   deque)
	  (else
	   (set-prev-ptr! (front-ptr deque) new-item)
	   (set-next-ptr! new-item (front-ptr deque))
	   (set-front-ptr! deque new-item)
	   deque))))
(define (rear-insert-queue! deque value)
  (let ((new-item (make-bidirect-item value)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-item)
	   (set-rear-ptr! deque new-item)
	   deque)
	  (else
	   (set-prev-ptr! new-item (rear-ptr deque))
	   (set-next-ptr! (rear-ptr deque) new-item)
	   (set-rear-ptr! deque new-item)
	   deque))))
(define (front-delete-queue! deque)
  (cond ((empty-deque? deque)
	 (error "FRONT-DELETE! called with an empty queue" deque))
	(else (if (null? (next-ptr (front-ptr deque)))
		  (set-rear-ptr! deque ())
		  (set-prev-ptr! (next-ptr (front-ptr deque)) ()))
	      (set-front-ptr! deque (next-ptr (front-ptr deque)))
	      deque)))
(define (rear-delete-queue! deque)
  (cond ((empty-deque? deque)
	 (error "REAR-DELETE! called with an empty queue" deque))
	(else (if (null? (prev-ptr (rear-ptr deque)))
		  (set-front-ptr! deque ()) 
		  (set-next-ptr! (prev-ptr (rear-ptr deque)) ()))
	      (set-rear-ptr! deque (prev-ptr (rear-ptr deque)))
	      deque)))
(define (print-deque deque)
  (define (iter-value bidirect)
    (cond ((null? bidirect) ())
	  (else (cons (get-value bidirect) (iter-value (next-ptr bidirect)))))) 
   (iter-value (front-ptr deque)))
  
(define d1 (make-deque))
(front-insert-queue! d1 1)
(print-deque d1) ;; (1)
(front-insert-queue! d1 2)
(print-deque d1) ;; (2 1)
(rear-insert-queue! d1 4)
(print-deque d1) ;; (2 1 4)
(front-delete-queue! d1)
(print-deque d1) ;; (1 4)
(rear-delete-queue! d1)
(print-deque d1) ;; (1)
(rear-delete-queue! d1)
(print-deque d1) ;; ()

;;ex3.24
(define (assoc-if key records predicate)
  (cond ((null? records) #f)
	((predicate key (caar records)) (car records))
	(else (assoc-if key (cdr records) predicate))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-if key-1 (cdr local-table) same-key?)))
	(if subtable
	    (let ((record (assoc-if key-2 (cdr subtable) same-key?)))
	      (if record (cdr record) #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-if key-1 (cdr local-table) same-key?)))
	(if subtable
	    (let ((record (assoc-if key-2 (cdr subtable) same-key?)))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
	    (set-cdr! local-table (cons (list key-1 (cons key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: TABLE" m))))
    dispatch))
    
;;ex3.25
(define (assoc key records)
  (cond ((null? records) #f)
	((eq? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (make-multikey-table)
  (let ((local-table (list (list '*table*))))
    (define (lookup-table table key) (assoc key (cdr table)))
    (define (lookup keys table)
      (cond ((null? keys) #f)
	    ((null? (cdr keys))
	     (let ((record (lookup-table table (car keys))))
	       (if record (cdr record) #f)))
	    (else
	     (let ((subtable (lookup-table table (car keys))))
	       (if subtable (lookup (cdr keys) subtable) #f)))))
    (define (insert! keys value table)
      (cond ((null? keys) (error "Key is null: INSERT"))
	    ((null? (cdr keys))
	     (let ((record (lookup-table table (car keys))))
	       (if record
		   (set-cdr! record value)
		   (set-cdr! table (cons (cons (car keys) value) (cdr table))))))
	    (else
	     (let ((subtable (lookup-table table (car keys))))
	       (if subtable
		   (insert! (cdr keys) value subtable)
		   (let ((newtable (list (car keys))))
		     (insert! (cdr keys) value newtable)
		     (set-cdr! table (cons newtable (cdr table))))))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
	    ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys value local-table)))
	    (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define mt (make-multikey-table))

;;ex3.26
;;(キー、値)のレコードを要素とし、キーをもとに二分木を構築する
;;辞書の探索にはlookup、挿入にはadjoin-setを使う

;;ex3.27
(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
	   ((= n 1) 1)
	   (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))
;;(memo-fib 3)
;;grobal-env->[memo-fib memoize]
;;             |
;;     memoize-(body, e)->[table, x:3]
;;(memo-fib 2) -> (+ 1 0)
;;grobal-env->[memo-fib memoize]
;;             |
;;     memoize-(body, e)->[table, x:2]
;;(memo-fib 1)
;;grobal-env->[memo-fib memoize]
;;             |
;;     memoize-(body, e)->[table(1), x:1]
;;(memo-fib 0)
;;grobal-env->[memo-fib memoize]
;;             |
;;     memoize-(body, e)->[table(1), x:0]
;;(memo-fib 1) -> 1
;;grobal-env->[memo-fib memoize]
;;             |
;;     memoize-(body, e)->[table((1,1)(0,0)), x:2]
;;(+ (+ 1 0) 1)
;; 2
;;(memoize fib)ではうまくいかない。
;;再帰呼出しにてその結果をメモする必要があるが、上記では再帰呼び出しがメモされない。

;;ex3.28
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! and-action-procedure a1)
  (add-action! and-action-procedure a2)
  'ok)
(define (logical-and s1 s2)
  (cond ((= s1 1) (= s2 1) 1)
	((= s1 1) (= s2 0) 0)
	((= s1 0) (= s2 1) 0)
	((= s1 0) (= s2 0) 0)
	(else (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! or-action-procedure a1)
  (add-action! or-action-procedure a2)
  'ok)
(define (logical-or s1 s2)
  (cond ((= s1 1) (= s2 1) 1)
	((= s1 1) (= s2 0) 1)
	((= s1 0) (= s2 1) 1)
	((= s1 0) (= s2 0) 0)
	(else (error "Invalid signal" s1 s2))))

;;ex3.29
(define (or-gate a1 a2 output)
  (let ((b1 (make-wire)) (b2 (make-wire)) (c (make-wire)))
    (inverter a1 b1)
    (inberter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))
;;遅延時間=and-gate-delay+2*inverter-delay

;;ex3.30
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
(define (ripple-carry-adder a b s c)
  (cond ((null? a) (error "Invalid Input"))
	((null? (cdr a)) (full-adder (car a) (car b) 0 (car s) c))
	(else (let ((c-out (make-wire)))
		(ripple-carry-adder (cdr a) (cdr b) (cdr s) c-out)
		(full-adder (car a) (car b) c-out (car s) c)))))
;; half-adder max(or*2+inv, and+or)
;; full-adder 2*half + or
;; ripple-carry-adder n*full = 2*n*max(or*2+inv, and+or)+or
  
;;ex3.31
(define (make-wire)
  (let ((signal-value 0) (action-procedures ()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons porc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown opeartion: WIRE" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
	     (call-each (cdr procedures)))))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda)) action the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name) (display " ")
		 (display (current-time the-agenda))
		 (display " New-value = ") (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;;初期化がないと、after-delayが呼び出されず、アクションのagendaへの登録がされない。

;;ex3.32
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments)) action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr! segments (cons (make-new-time-segment time action) (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments! agenda (cons (make-new-time-segment time action) segments))
	(add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-items agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))

;;キューでなくスタックにすると(1->0)->(0->1)の順に処理され、間違った結果になる。

;;ex3.33
(define (averager a b c)
  (let ((u (make-connector))
	(v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))

;;ex3.34
;;multiplierは3つの値のうち2つが定まらないと残り1つが定まらない。
;;ゆえにaが不定な場合、b1つだけが定まることとなるので、aを定めることができない。

;;ex3.35
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2)) (set-value! sum (+ (get-value a1) (get-value a2)) me))
	  ((and (has-value? a1) (has-value? sum)) (set-value! a2 (- (get-value sum) (get-value a1)) me))
	  ((and (has-value? a2) (has-value? sum)) (set-value! a1 (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product (* (get-value m1) (get-value m2)) me))
	  ((and (has-value? product) (has-value? m1))
	   (set-value! m2 (/ (get-value product) (get-value m1)) me))
	  ((and (has-value? product) (has-value? m2))
	   (set-value! m1 (/ (get-value product) (get-value m2)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! product me)
    (forget-value! product me)
    (process-new-value))
 (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknonwn request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ") (display name) (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f)
	(informant #f)
	(constraints ()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter inform-about-value constraints))
	    ((not (= value newval))
	     (error "Contradiction" (list value newval)))
	    (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin (set! informant #f)
		 (for-each-except retractor inform-about-no-value constraints))
	  'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	  (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
	  (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant #t #f))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown operation: CONNECTOR" request))))
    me))
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) exception) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))
 
(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant) ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
	(if (< (get-value b) 0)
	    (error "square less than 0: SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(if (has-value? a)
	    (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;;ex3.36
;;global-env->[a, inform-about-value, make-connector, for-each-except]
;;             |
;;            (body, env)->[value:10, informant:'user, constraints:()]

;;ex3.37
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
	  x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))
(define (cv c)
  (let ((z (make-connector)))
    (constant c z)
    z))

;;ex3.38
;;a
;;Paul->Peter->Mary 45
;;Paul->Mary->Peter 35
;;Peter->Paul->Mary 45
;;Peter->Mary->Paul 50
;;Mary->Peter->Paul 40
;;Mary->Paul->Peter 40
;;b
;;   Peter Paul Mary Acc
;;   100   100  100  100
;;1. 110             110 <-Peter
;;2.        80        80 <-Paul
;;3.             50   50 <-Mary
;;         110  110  110 <-Peter      
;;4.        90           <-Paul
;;5.             55      <-Mary
;;    80         80   80 <-Paul
;;6.  90              90 <-Peter
;;7.             45   45 <-Paul
;;    50    50        50 <-Mary
;;8.  60              60 <-Peter
;;9.        30        30 <-Mary

;;ex3.39
;;1. プロセスに割り込みなく以下の順に実行されると、101
(set! x (s (lambda () (* x x))))
(s (lambda () (set! x (+ x 1))))
;;2. プロセスに割り込みなく以下の順に実行されると、121
(s (lambda () (set! x (+ x 1))))
(set! x (s (lambda () (* x x))))
;;3. (s (lambda () (* x x))))->(s (lambda () (set! x (+ x 1))))->(set! x (...))
;;となった場合、100
(set! x (s (lambda () (* x x))))
(s (lambda () (set! x (+ x 1))))

;;ex3.40
;;100
;;1000
;;1000000<-serialized

;;ex3.41
;;balanceを直列化しなくても異常なふるまいは起こらない。
;;読み出しは並行に行っても異常なふるまいとなることはない。

;;ex3.42
;;シリアライザにより直列化された手続きを再利用するか、毎回生成するかで、並行性に違いはない。

;;ex.3.43
;;逐次実行の場合、各口座の残高が入れ替わるだけなので、何らかの順番で10,20,30である。
;;最初のバージョンのexchange
;;  a  b    p1           p2
;; 10 20   
;; 10 10    withdraw 10
;; 10  0                 withdraw 10
;; 20  0    deposite 10
;; 30  0                 deposite 10
;;exchange内の各口座への処理順序が他のプロセスと入り混じる場合でも、各口座単位での処理は逐次的に行われるため、合計額が変わることはない。
;;各口座単位での処理が直列化されていない場合
;;  a  b    p1           p2
;; 10 20    - 20 10
;; 10 20                - 20 10
;; 10 10    set! b 10      
;; 10 10                 set! b 10
;; 10 10    + 10 10 
;; 20 10    set! a 20
;; 20 10                 + 20 10
;; 30 10                 set! b 30
;; set!する対象の口座が別のプロセスが変更し終える前だと、合計額が変わる。

;;ex3.44
;;正しくない
;;Exchangeは残高取得を伴うが、Transferは残高取得がない
;;Transferは直列化された各口座への操作を逐次実行することになるので、Transfer自体を直列化する必要はない。

;;ex3.45
;;(serializer1 (serializer2 (serializer1 withdraw)))となり、最内の手続きが最外の手続きが終わるのを待つが、最外の手続きが実行中であるため、待ち続けてしまう。

;;ex3.46
;;a              b          cell
;;(car cell)                    #f
;;               (car cell)     #f
;;(set! cell #t)                #t
;;               (set! cell #t) #t

;;ex3.47
(define (make-semaphore n)
  (let ((mutex (make-mutex))
	(count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (<= count n)
		 (begin (mutex 'acquire)
			(set! count (+ count 1))
			(mutex 'release))
		 (the-semaphore m)))
	    ((eq? m 'release)
	     (mutex 'acquire)
	     (set! count (- count 1))
	     (mutex 'release))))
    the-semaphore))

(define (make-semaphore2 n)
  (let ((cell (list false))
	(count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (<= count n)
		 (if (test-and-set! cell)
		     (the-semaphore m)
		     (begin (set! count (+ count 1))
			    (clear! cell)))
		 (the-semaphore m)))
	    ((eq? m 'release)
	     (if (test-and-set! cell)
		 (the-semaphore m)
		 (begin (set! count (- count 1))
			(clear! cell))))))
    the-semaphore))

;;ex3.48
;;複数口座に順序付けをもたせることで、互いが互いのリソースを必要とする状況がなくなるため、デッドロックしなくなる。
(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! (balance (-balance amouont))) balance)
	"Insufficient funds"))
  (define (deposite amount)
    (set! balance (+ balance amount)) balance)
  (let ((balance-serializer (make-serializer))
	(account-id id))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
	    ((eq? m 'deposite) (balance-serializer deposite))
	    ((eq? m 'balance) balance)
	    ((eq? m 'serializer) balance-serializer)
	    ((eq? m 'id) account-id)
	    (else (errpr "Unknown request:MAKE-ACCOUNT" m))))
    dispatch))
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposite) difference)))		    
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer))
	(id1 (account1 'id))
	(id2 (account2 'id)))
    (if (< id1 id2)
	((serializer2 (serializer1 exchange)) account2 account1)
	((serializer1 (serializer2 exchange)) account1 account2))))
;;ex3.49
;;事前にロックすべき口座が全てわかっていない場合、適切なロック順序とならず、デッドロックが起こりうる。

(define the-empty-stream ())
(define (stream-null? s) (null? s)) 
(define-syntax cons-stream
  (syntax-rules ()
    [(_ a d) (cons a (delay d))]))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream)) (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))
(define (stream-head s n)
  (define (iter s n)
    (if (= n 0)
	'done
	(begin
	  (display (stream-car s))
	  (newline)
	  (iter (stream-cdr s) (- n 1)))))
  (iter s n))

;;ex3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))
			  
;;ex3.51
(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
;;0x
(stream-ref x 5)
;;1
;;2
;;3
;;4
;;55
(stream-ref x 7)
;;6
;;77

;;cons-streamで結合した要素がstream-cdrで取り出される際に、showが実行される。つまりdisplay-lineが遅延実行される。

;;ex3.52
(define sum 0) ;;sum:0
(define (accum x) (set! sum (+ x sum)) sum) ;;sum:0
(define seq (stream-map accum (stream-enumerate-interval 1 20))) ;;sum:1
(define y (stream-filter even? seq)) ;;sum:6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq)) ;;sum:10
(stream-ref y 7) ;;sum:136(sum of 1~16)
(display-stream z) ;;sum:210(sum of 1-20)
;;メモ化しないと結果が変わる。delayを一度評価した後、再度評価するとaccumが実行され、sumに再度要素との和が代入されることになる。
;;メモ化した場合は、sumは1から評価した要素の位置までの和が維持されるが、メモ化しないと上記により実行のたびにsumが増加する。

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7))) integers))
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(define (sieve stream)
  (cons-stream (stream-car stream)
	       (sieve (stream-filter (lambda (x) (not (divisible? x (stream-car stream)))) (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))) 
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(define primes
  (cons-stream 2
	       (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
	  ((divisible? n (stream-car ps)) #f)
	  (else (iter (stream-cdr ps)))))
  (iter primes))
	   
;;ex3.53
(define s (cons-stream 1 (add-streams s s)))
;; 1 2 4 8...
;; 一つ前の要素を二倍する、つまり2のべき乗になる。

;;ex3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))

;;ex3.55
(define (partial-sums s) (cons-stream (stream-car s) (stream-map + (partial-sums s) (stream-cdr s))))

;;ex3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car) (cons-stream s2car (merge (stream-cdr s2) s1)))
		 (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))
		      
;;ex3.57
;;n=1 
;;n=2 (+ 1 0)
;;n=3 (+ 1(=メモ化された3番目の1) 1(=2番目の1))
;;n=4 (+ 2(=メモ化された4番目の2) 1(=メモ化された3番目の1)
;;n番目の計算にn-1回の加算が行われる
;;メモ化がないと、メモ化されていた要素が遡って計算される。その回数はフィボナッチ数列に等しいため、指数的に増加する。

;;ex3.58
(define (expand num den radix)
  (cons-stream (quotient (* num radix) den)
	       (expand (remainder (* num radix) den) den radix)))
;;numとradixの積をdenで割った商を返す。
;;numをnumとradixの積をdenで割った余りに置き換えて、繰り返す
;;(expand 1 7 10) 1 4 2 8 5 7 1...
;;(expand 3 8 10) 3 7 5 0
;;radixを基数とした、numをdenで割り算した値の各桁の数列

;;ex3.59
(define (integrate-series a)
  (stream-map / a integers))
(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define cosine-series (cons-stream 1 (stream-map - (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

;;ex3.60
;;a0+a1x+a2x^2
;;b0+b1x+b2x^2
;;x^2=a0*b2+b1*a1+a2*b0
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))

;;ex3.61
(define (invert-unit-series a)
  (define x (cons-stream 1 (mul-series (stream-map - (stream-cdr a)) x)))
  x)

;;ex3.62
(define (div-series s1 s2)
  (cond ((= (stream-car s2) 0) (error "denominator constant is zero"))
	(else (mul-series s1 (invert-unit-series s2)))))
(define tangent-series (div-series sine-series cosine-series))
;;0 1 0 1/3 0 2/15...

;;ex3.63
;;sqrt-streamの結果がメモ化されず、stream-cdrのたびに実行される。
;;delayがメモ化されていなければ、どちらのバージョンも同じ計算量となる。

;;ex3.64
(define (stream-limit s tolerance)
  (let ((e1 (stream-car s))
	(e2 (stream-car (stream-cdr s))))
    (if (<= (abs (- e1 e2)) tolerance)
	e2
	(stream-limit (stream-cdr s) tolerance))))
(define (average a b)
  (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses (cons-stream 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;ex3.65
(define (ln-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (ln-summands (+ n 1)))))
(define ln-stream (partial-sums (ln-summands 1)))
;;1.0
;;0.5
;;0.8333333333333333
;;0.5833333333333333
;;0.7833333333333332
;;0.6166666666666666
;;0.7595238095238095
;;0.6345238095238095
;;0.7456349206349207
;;0.6456349206349207
;;...
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))
(display-stream (euler-transform ln-stream))
;;0.7
;;0.6904761904761905
;;0.6944444444444444
;;0.6924242424242424
;;0.6935897435897436
;;0.6928571428571428
;;0.6933473389355742
;;0.6930033416875522
;;0.6932539682539683
;;0.6930657506744464
;;...
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
(display-stream (accelerated-sequence euler-transform ln-stream))
;;1.0
;;0.7
;;0.6932773109243697
;;0.6931488693329254
;;0.6931471960735491
;;0.6931471806635636
;;0.6931471805604039
;;0.6931471805599445
;;0.6931471805599427
;;0.6931471805599454

;;ex3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
	       (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
			   (pairs (stream-cdr s) (stream-cdr t)))))
;;(1　100) 2*99-1=197
;;(i, j)に対して
;;i=j -> 2^i-2
;;j-1=i -> index(i, j-1)+2^(i-1)
;;j-1>i -> index(i, j-1)+2^i
;;(99 100) 2^99-2+2^98=950,737,950,171,172,051,122,527,404,030
;;(100 100) 2^100-2=1,267,650,600,228,229,401,496,703,205,374

;;ex3.67
(define (pairs2 s t)
  (cons-stream (list (stream-car s) (stream-car t))
	       (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
			   (interleave (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
				       (pairs2 (stream-cdr s) (stream-cdr t))))))
