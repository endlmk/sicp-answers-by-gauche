(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b (test (op =) (reg b) (const 0))
	    (branch (label gcd-done))
	    (assign t (op rem) (reg a) (reg b))
	    (assign a (reg b))
	    (assign b (reg t))
	    (goto (label test-b))
	    gcd-done)))
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(get-register-contents gcd-machine 'a)

;;ex5.7
(define expt-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '- -) (list '* *))
   '(test-expt (assign continue (label expt-done))
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
	       expt-done)))
(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 4)
(start expt-machine)
(get-register-contents expt-machine 'val)

;;ex5.8
(controller
 start
 (goto (label here))
 here
 (assign a (const 3))
 (goto (label there))
 here
 (assign a (const 4))
 (goto (label there))
 there)
	 
;;後のhereがラベルとして有効になる。そのため、aは4になる

(define (extract-labels text recieve)
  (if (null? text)
      (recieve () ())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
	 (let ((next-inst (car text)))
	   (if (symbol? next-inst)
	       (let ((label (assoc next-inst labels)))
		 (if label
		     (error "Already defined label: ASSEMBLE" label)
		     (recieve insts
			      (cons (make-label-entry next-inst insts)
				    labels))))
		 (recieve (cons (make-instruction next-inst)
				insts)
			  labels)))))))
