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
	    
