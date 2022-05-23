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
 expt-done)

(controller
 test-counter
 (test (op =) (reg counter) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1))
 (assign product (op *) (reg b) (reg product))
 (goto (lebel test-counter))
 expt-done)


