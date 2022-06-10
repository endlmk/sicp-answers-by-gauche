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
;;ex5.9
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
			 operations))
	(aprocs
	 (map (lambda (e)
		(if (label-exp? e)
		    (error "Invalid operation to label: ASSEMBLE" e)
		    (make-primitive-exp e machine labels)))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;;ex5.10
;;適切な構文が思い浮かばないのでパス
;;make-execution-procedureに追加すれば他の処理は変更しなくてよい

;;ex5.11
;;a
;; (assign n (reg val)) n=val=Fib(n-2)
;; (restore val) n=Fib(n-2) val=Fib(n-1)
;;...
;; (assign val (op +) (reg val) (reg n)) Fib(n-1)+Fib(n-2)
;;以下に変更可能、nとvalが入れ替わるが加算の結果は変わらない
;; (restore n) n=Fib(n-1) val=Fib(n-2)
;; (assign val (op +) (reg val) (reg n)) Fib(n-1)+Fib(n-2)

;;b
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (let ((reg-name-val-pair (pop stack)))
	(if (eq? (car reg-name-val-pair) (stack-inst-reg-name inst))
	    (begin (set-contents! reg (cdr reg-name-val-pair))
		   (advance-pc pc))
	    (error "Different register name: ASSEMBLE" inst))))))
;;c
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name)
       ((machine 'allocate-stack) register-name)) ;;mod
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack ()) ;;mod
	(the-instruction-sequence ()))
    (let ((the-ops
	   (list (list 'initialize-stack ;;mod
		       (lambda () (map ;;mod
				   (lambda (reg-stack-pair) ((cadr reg-stack-pair) 'initialize)) ;;mod
				   stack))))) ;;mod
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (allocate-stack name) ;;mod
	(if (assoc name stack)
	    (error "Multiply defined register: " name)
	    (set! stack
		  (cons (list name (make-stack))
			stack)))
	'stack-allocated)
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'allocate-stack)
	       allocate-stack)
	      (else (error "Unknown request: Machine" message))))
      dispatch)))

(define (make-save inst machine reg-stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name))
	  (reg-stack-list (assoc reg-name reg-stack)))
      (if reg-stack-list
	  (lambda ()
	    (push (cadr reg-stack-list) (get-contents reg))
	    (advance-pc pc))
	  (error "Unknown reg-stack: ASSEMBLE" inst)))))
(define (make-restore inst machine reg-stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name))
	  (reg-stack-list (assoc reg-name reg-stack)))
      (if reg-stack-list
	  (lambda ()
	    (set-contents! reg (cadr reg-stack-list))
	    (advance-pc pc))
	  (error "Unknown reg-stack: ASSEMBLE" stack)))))
	    
;;ex5.12
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    (machine 'init-analyze-seq) ;;mod
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(analyze-sequence ()) ;;mod
	(the-instruction-sequence ()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (init-analyze-seq) ;;mod
	(set! analyze-sequence (map (lambda (x) (list x))
				    (list 'assign 'test 'branch 'goto 'save 'restore 'perform
					  'goto-regs 'save-regs 'restore-regs 'source))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'get-analyze-seq) analyze-sequence) ;;mod
	      ((eq? message 'init-analyze-seq) (init-analyze-seq)) ;;mod
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request: Machine" message))))
      dispatch)))
(define (analyze-machine machine)
  (machine 'get-analyze-seq))

(define (insert-analyze-seq! exp machine type)
  (let ((record
	 (cond ((eq? type 'inst) (assoc (car exp) (machine 'get-analyze-seq)))
	       ((eq? type 'goto) (assoc 'goto-regs (machine 'get-analyze-seq)))
	       ((eq? type 'save) (assoc 'save-regs (machine 'get-analyze-seq)))
	       ((eq? type 'restore) (assoc 'restore-regs (machine 'get-analyze-seq)))
	       ((eq? type 'source) (assoc 'source (machine 'get-analyze-seq))))))
    (if record
	(if (eq? type 'source)
	    (let ((target (assoc (assign-reg-name exp) (cdr record))))
	      (if target
		  (if (not (member (assign-value-exp exp) (cdr target)))
		      (set-cdr! target (cons (assign-value-exp exp) (cdr target))))
		  (set-cdr! record (cons (list (assign-reg-name exp) (assign-value-exp exp)) (cdr record)))))
	    (if (not (member exp (cdr record)))
		(set-cdr! record (cons exp (cdr record)))))
	(error "Analyze error: " exp))))
		
(define (make-assign inst machine labels operations pc)
  (insert-analyze-seq! inst machine 'inst) ;;mod
  (insert-analyze-seq! inst machine 'source) ;;mod
  (let ((target (get-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp value-exp machine labels operations)
	       (make-primitive-exp (car value-exp) machine labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))
(define (make-test inst machine labels operations flag pc)
  (insert-analyze-seq! inst machine 'inst) ;;mod
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc (make-operation-exp condition machine labels operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc pc)))
	(error "Bad TEST instruction: ASSEMBLE" inst))))
(define (make-branch inst machine labels flag pc)
  (insert-analyze-seq! inst machine 'inst) ;;mod
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts (lookup-label labels (label-exp-label dest))))
	  (lambda ()
		  (if (get-contents flag)
		      (set-contents! pc insts)
		      (advance-pc pc))))
	(error "Bad-BRANCH instruction: ASSEMBLE" inst))))
(define (make-goto inst machine labels pc)
  (insert-analyze-seq! inst machine 'inst) ;;mod
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts (lookup-label labels (label-exp-label dest))))
	     (lambda () (set-contents! pc insts))))
	  ((register-exp? dest)
	   (insert-analyze-seq! (register-exp-reg dest) machine 'goto) ;;mod
	   (let ((reg (get-register machine (register-exp-reg dest))))
	     (lambda () (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
(define (make-save inst machine stack pc)
  (insert-analyze-seq! inst machine 'inst) ;;mod
  (insert-analyze-seq! (stack-inst-reg-name inst) machine 'save) ;;mod
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (insert-analyze-seq! inst machine 'inst) ;;mod
  (insert-analyze-seq! (stack-inst-reg-name inst) machine 'restore) ;;mod
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (make-perform inst machine labels operations pc)
  (insert-analyze-seq! inst machine 'inst) ;;mod
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc (make-operation-exp action machine labels operations)))
	  (lambda () (action-proc) (advance-pc pc)))
	(error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define fib-machine
  (make-machine
   '(continue val n)
   (list (list '< <) (list '= =) (list '+ +) (list '- -))
   '(controller
     (assign continue (label fib-done))
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
   ))

;;ex5.13
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence ()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (begin (allocate-register name) ;;mod
		     (cadr (assoc name register-table)))))) ;;mod
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request: Machine" message))))
      dispatch)))

;;ex5.14
(define fact-machine-test
  (make-machine
   '(continue val n test-n)
    (list (list '= =) (list '- -) (list '* *))
    '(start
      (assign continue (label fact-done))
      fact-test-entry
      (test (op =) (reg test-n) (const 0))
      (branch (label fact-test-end))
      (perform (op initialize-stack))
      (assign n (reg test-n))	    
      fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done
      (perform (op print-statistics))
      (assign test-n (op -) (reg test-n) (const 1))
      (goto (label fact-test-entry))
      fact-test-end)
    ))
;;total-pushes, maximum-depthともに2*(n-1)となる

;;ex5.15
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence ())
	(instruction-count 0)) ;;mod
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))
		 (list 'print-statistics
		       (lambda () (stack 'print-statistics)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(set! instruction-count (+ instruction-count 1))
		(execute)))))
      (define (display-instruction-counting) ;;mod
	(newline)
	(display (list 'instruction-counting instruction-count))
	(set! instruction-count 0))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'display-instruction-counting) (display-instruction-counting)) ;;mod
	      (else (error "Unknown request: Machine" message))))
      dispatch)))

;;ex5.16
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence ())
	(instruction-count 0)
	(trace? #f)) ;;mod
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))
		 (list 'print-statistics
		       (lambda () (stack 'print-statistics)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		(if trace? ;;mod
		    (begin (newline)
			   (display (instruction-text (car insts))))) 
		((instruction-execution-proc (car insts)))
		(set! instruction-count (+ instruction-count 1))
		(execute)))))
      (define (display-instruction-counting)
	(newline)
	(display (list 'instruction-counting instruction-count))
	(set! instruction-count 0))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'display-instruction-counting) (display-instruction-counting))
	      ((eq? message 'trace-on) (set! trace? #t)) ;;mod
	      ((eq? message 'trace-off) (set! trace? #f)) ;;mod 
	      (else (error "Unknown request: Machine" message))))
      dispatch)))

;;ex5.17
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence ())
	(instruction-count 0)
	(trace? #f)
	(label '*no-label*)) ;;mod
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))
		 (list 'print-statistics
		       (lambda () (stack 'print-statistics)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(if (eq? (caaar insts) 'label) ;;mod
		    (set! label (cadaar insts))
		    (set! instruction-count (+ instruction-count 1)))
		(if trace? 
		    (print "label:" label ", instruction:" (instruction-text (car insts)))) ;;mod
		(execute)))))
      (define (display-instruction-counting)
	(newline)
	(display (list 'instruction-counting instruction-count))
	(set! instruction-count 0))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'display-instruction-counting) (display-instruction-counting))
	      ((eq? message 'trace-on) (set! trace? #t))
	      ((eq? message 'trace-off) (set! trace? #f))
	      (else (error "Unknown request: Machine" message))))
      dispatch)))

(define (extract-labels text recieve)
  (if (null? text)
      (recieve () ())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
	 (let ((next-inst (car text)))
	   (if (symbol? next-inst)
	       (let ((insts-label (cons (make-instruction (list 'label next-inst)) ;;mod
					insts)))
		     (recieve insts-label ;;mod
			      (cons (make-label-entry next-inst insts-label) ;;mod
				    labels)))
	       (recieve (cons (make-instruction next-inst)
			      insts)
			labels)))))))
	   
(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign) (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test) (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch) (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto) (make-goto inst machine labels pc))
	((eq? (car inst) 'save) (make-save inst machine stack pc))
	((eq? (car inst) 'restore) (make-restore inst machine stack pc))
	((eq? (car inst) 'perform) (make-perform inst machine labels ops pc))
	((eq? (car inst) 'label) (lambda () (advance-pc pc))) ;;mod
	(else (error "Unknown instruction type: ASSEMBLE" inst))))
