;;ex5.23
;;add eval-dispatch
(test (op cond?) (reg exp))
(branch (label ev-cond))
(test (op let?) (reg exp))
(branch (label ev-let))

ev-cond
(assign exp (op cond->if) (reg exp))
(goto (label eval-dispatch))
ev-let
(assign exp (op let->combination) (reg exp))
(goto (label eval-dispatch))

;;ex5.24
ev-cond
(assign unev (op cond-clauses) (reg exp))
(save continue)
ev-cond-loop
(test (op null?) (reg unev))
(branch (label ev-cond-null))
(assign exp (op car) (reg unev))
(test (op cond-else-clause?) (reg exp))
(branch (label ev-cond-action))
(save exp)
(assign exp (op cond-predicate) (reg exp))
(save unev)
(save env)
(assign continue (label ev-cond-decide))
(goto (label eval-dispatch))
ev-cond-decide
(restore env)
(restore unev)
(restore exp)
(test (op true?) (reg val))
(branch (label ev-cond-action))
(assign unev (op cdr) (reg unev))
(goto (label ev-cond-loop))
ev-cond-null
(restore continue)
(assign val (const #f))
(goto (reg continue))		
ec-cond-action
(assign unev (op cond-action) (reg exp))
(goto (label ev-sequence))

;;ex5.25
;;skip

;;ex5.26
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product) (+ counter 1))))
  (iter 1 1))

;;maximum-depth: 10
;;total-pushes: 35*n+29

;;ex5.27
(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))

;;maximum-depth: 5*n+3
;;total-pushes: 32*n-16

;;ex5.28
;;iterative
;;maximum-depth: 3*n+14
;;total-pushes: 37*n+33

;;recursive
;;maximum-depth: 8*n+3
;;total-pushes: 34*n-16

;;ex5.29
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
;;a
;;maximum-depth: 5*n+3

;;b
;;n=3  128
;;n=4  240
;;n=5  408 = S(4)+S(3)+40
;;n=6  688 = S(5)+S(4)+40
;;n=7 1136 = S(6)+S(5)+40

;;k=40

;;S(3)(=128)=a*fib(4)(=3)+b
;;S(4)(=240)=a*fib(5)(=5)+b
;;S(5)(=408)=a*fib(6)(=8)+b
;;S(6)(=688)=a*fib(7)(=13)+b

;;a=56
;;b=-40

;;ex5.30
;;skip
