;;ex5.1
;;skip

;;ex5.2
(controller
 test-counter
 (test (op >) (reg counter) (reg n))
 (barnch (label factorial-done))
 (assign product (op *) (reg product) (reg counter))
 (assign counter (op +) (reg counter) (const 1))
 (goto (label test-counter))
 factorial-done)
       
;;ex5.3
(controller
 test-good-enough
 (test (op good-enough?) (reg guess))
 (branch (label sqrt-done))
 (assign guess (op improve) (reg guess))
 (goto (label test-good-enough))
 sqrt-done)

(controller
 test-good-enough
 (assign t (op square) (reg guess))
 (assign t (op -) (reg t) (reg x))
 (assign t (op abs) (reg t))
 (test (op <) (reg t) (const 0.001))
 (branch (label sqrt-done))
 (assign t (op /) (reg x) (reg guess))
 (assign t (op average) (reg guess) (reg t))
 (assign guess (reg t))
 (goto (label test-good-enough))
 sqrt-done)

;;ex5.4
(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (save n)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore n)
 (restore continue)
 (assign val (op *) (reg b) (reg val))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
 expt-done)

(controller
 test-counter
 (test (op =) (reg counter) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1))
 (assign product (op *) (reg b) (reg product))
 (goto (lebel test-counter))
 expt-done)

;;ex5.5
(controller
 (assign continue (label fact-done))
 fact-loop
 (test (op =) (reg n) (const 1))
 (branch (label base-case))
 (save continue)
 (save n)
 (assgin n (op -) (reg n) (const 1))
 (assgin continue (label after-fact))
 (goto (label fact-loop))
 after-fact
 (restore n)
 (restore continue)
 (assign val (op *) (reg n) (reg val))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (doto (reg continue))
 fact-done)

;;n=2
;;stack ((label fact-done))
;;stack (2 (label fact-done))
;;stack (2 (label fact-done))
;;n=1
;;continue=(label after-fact)
;;val=1
;;n=2
;;continue=(label fact-done)
;;val = 2*1=2
;;fact-done

(controller
 (assgin continue (label fib-done))
 fib-loop
 (test (op <) (reg n) (const 2))
 (branch (label immediate-answer))
 (save continue)
 (assign continue (label afterfib-n-1))
 (save n)
 (assign n (op -) (reg n) (const 1))
 (goto (label fib-loop))
 afterfib-n-1
 (restore n)
 (restore continue)
 (assign n (op -) (reg n) (const 2))
 (save continue)
 (assign continue (label afterfib-n-2))
 (save val)
 (goto (label fib-loop))
 afterfib-n-2
 (assign n (reg val))
 (restore val)
 (restore continue)
 (assign val (op +) (reg val) (reg n))
 (goto (reg continue))
 immediate-answer
 (assign val (reg n))
 (goto (reg continue))
 fib-done)

;;n=4
;;s ((label fib-done))
;;continue=(label afterfib-n-1)
;;s (4 (label fib-done))
;;n=3
;;s ((label afterfib-n-1) 4 (label fib-done))
;;s (3 (label afterfib-n-1) 4 (label fib-done))
;;n=2
;;s ((label afterfib-n-1) 3 (label afterfib-n-1) 4 (label fib-done))
;;s (2 (label afterfib-n-1) 3 (label afterfib-n-1) 4 (label fib-done))
;;n=1
;;val=1
;;n=2
;;s (3 (label afterfib-n-1) 4 (label fib-done))
;;n=0
;;s ((label afterfib-n-1) 3 (label afterfib-n-1) 4 (label fib-done))
;;continue=(label afterfib-n-2)
;;s (1 (label afterfib-n-1) 3 (label afterfib-n-1) 4 (label fib-done))
;;val=0
;;n=0
;;val=1
;;continue=(label afterfib-n-1)
;;s (3 (label afterfib-n-1) 4 (label fib-done))
;;val=1
;;n=3
;;n=1
;;s ((label afterfib-n-1) 4 (label fib-done))
;;continue=(label afterfib-n-2)
;;s (1 (label afterfib-n-1) 4 (label fib-done))
;;val=1
;;n=1
;;val=1
;;continue=(label afterfib-n-1
;;s (4 (label fib-done))
;;val=2
;;n=4
;;n=2
;;s ((label fib-done))
;;continue=(label afterfib-n-2)
;;s (2 (label fib-done))
;;s ((label afterfib-n-2) 2 (label fib-done))
;;continue=(label afterfib-n-1)
;;s (2 (label afterfib-n-2) 2 (label fib-done))
;;n=1
;;val=1
;;n=2
;;n=0
;;s ((label afterfib-n-2) 2 (label fib-done))
;;continue=(label afterfib-n-2)
;;s (1 (label afterfib-n-2) 2 (label fib-done))
;;val=0
;;n=0
;;val=1
;;continue=(label afterfib-n-2)
;;s (2 (label fib-done))
;;val=1
;;n=1
;;val=2
;;continue=(label fib-done)
;;s ()
;;val=3
;;fib-done

;;ex5.6
;; afterfib-n-1
;; (restore n)
;; (restore continue);;不要
;; (assign n (op -) (reg n) (const 2))
;; (save continue);;不要
