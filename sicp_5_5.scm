;;ex5.31
;;(a) 演算子の評価の前後でenvの保存と復元を行う
;;(b) 各被演算子の評価の前後でenvの保存と復元を行う
;;(c) 各被演算子の評価の前後でarglの保存と復元を行う
;;(d) 被演算子列の評価の前後でprocの保存を復元を行う

(f 'x 'y)     ;;(a)~(d)すべて除去可能。いずれのレジスタも書き換えないため
((f) 'x 'y)   ;;(a)~(d)すべて除去可能。(f)で(a)は書き換わるが引数はシンボルのため影響がないため
(f (g 'x) y)  ;;(a)のみ除去可能。引数内で適用があるため(c)(d)は必要。引数内の適用によりyが影響を受ける可能性があるので(b)も必要。
(f (g 'x) 'y) ;;(a)(b)は除去可能。引数内で適用があるため(c)(d)は必要。引数内の適用はシンボル'yに影響がないため(b)は不要。

;;ex5.32
;;a
ev-application
(save continue)
(assign unev (op operands) (reg exp))
(assign exp (op operator) (reg exp))
(test (op symbol?) (reg exp))
(branch (label ev-appl-symbol-operator))
(save env)
(save unev)
(assign continue (label ev-appl-did-operator))
(goto (label eval-dispatch))
ev-appl-symbol-operator
(assign continue (label ev-appl-did-symbol-operator))
(goto (label eval-dispatch))
ev-appl-did-symbol-operator
(assign argl (op empty-arglist))
(assign proc (reg val))
(test (op no-operands?) (rev unev))
(branch (label apply-dispatch))
(save proc)
(goto (label ev-appl-operand-loop))

;;b
;;逐次評価である以上、コンパイラのようにコード全体がわかった上での最適化を行うことはできない。

;;ex5.33
;;factorial
(env)
(val)
  (assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
compiled-branch7
  (assign continue (label after-call8))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call8
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
true-branch3
  (assign val (const 1))
  (goto (reg continue))
false-branch4
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch9))
compiled-branch10
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch9
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call11
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
compiled-branch13
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch15))
compiled-branch16
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch15
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call17
after-if5
after-lambda2
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
#<undef>

;;factorial-alt
(env)
(val)
  (assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
compiled-branch7
  (assign continue (label after-call8))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call8
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
true-branch3
  (assign val (const 1))
  (goto (reg continue))
false-branch4
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch9))
compiled-branch10
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch9
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call11
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch12))
compiled-branch13
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch12
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch15))
compiled-branch16
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch15
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call17
after-if5
after-lambda2
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
#<undef>

;;factorialでは先にnを評価してから再帰呼び出しするのに対し、factorial-altでは先に再帰呼び出ししてからnを評価する
;;命令数に相違はないので、実行効率は変わらない

;;ex5.34

(env)
(val)
  (assign val (op make-compiled-procedure) (label entry8) (reg env))
  (goto (label after-lambda9))
entry8
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry10) (reg env))
  (goto (label after-lambda11))
entry10
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch15))
compiled-branch16
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch15
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch13))
true-branch12
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch13
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch21))
compiled-branch22
  (assign continue (label after-call23))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch21
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call23
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch18))
compiled-branch19
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch18
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch24))
compiled-branch25
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch24
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call26
after-if14
after-lambda11
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch27))
compiled-branch28
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch27
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call29
after-lambda9
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
#<undef>

;;再帰版は、再帰の度にスタックを積み上げる
;;手続き版は、繰り返しの間でスタックは一定の深さになる

;;ex5.35
(define (f x) (+ x (g (+ x 2))))

;;ex5.36
;;右から左の順になる。construct-arglistで引数をreverseしてから処理するため。

;;reverseをやめ、consをappendにする
;;consよりappendのほうが効率が悪いので、効率は悪くなる
(define (construct-arglist operand-codes)
  ;;operandのreverseを削除
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
				 '((assign argl (const ()))))
      (let ((code-to-get-last-arg
	     (append-instruction-sequences
	      (car operand-codes)
	      (make-instruction-sequence '(val) '(argl)
					 '((assign argl (op list) (reg val)))))))
	(if (null? (cdr operand-codes))
	    code-to-get-last-arg
	    (preserving '(env)
			code-to-get-last-arg
			(code-to-get-rest-args
			 (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
	 (preserving '(argl)
		     (car operand-codes)
		     (make-instruction-sequence '(val argl) '(argl)
						'((assign val (op list) (reg val)) ;;appendのためlistにする
						  (assign argl
							  (op append) (reg val) (reg argl))))))) ;;consをappendにする
    (if (null? (cdr operand-codes))
	code-for-next-arg
	(preserving '(env)
		    code-for-next-arg
		    (code-to-get-rest-args (cdr operand-codes))))))

;;ex5.37
;;最適化なしバージョン
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
	(preserving (cdr regs)
		    (make-instruction-sequence
		     (list-union (list first-reg)
				 (registers-needed seq1))
		     (list-difference (registers-modified seq1)
				      (list first-reg))
		     (append `((save ,first-reg))
			     (statements seq1)
			     `((restore ,first-reg))))
		    seq2))))

;;無駄なsave,restoreが増えている
(continue env)
(val)
  (save continue)
  (save env)
  (save continue)
  (assign val (op make-compiled-procedure) (label entry18) (reg env))
  (restore continue)
  (goto (label after-lambda19))
entry18
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const 1))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (restore continue)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch23))
compiled-branch24
  (assign continue (label after-call25))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch23
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
after-call25
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch21))
true-branch20
  (save continue)
  (assign val (const 1))
  (restore continue)
  (goto (reg continue))
false-branch21
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const 1))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (restore continue)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch26))
compiled-branch27
  (assign continue (label after-call28))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch26
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
after-call28
  (assign argl (op list) (reg val))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch29))
compiled-branch30
  (assign continue (label after-call31))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch29
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
after-call31
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch32))
compiled-branch33
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch32
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))
after-call34
after-if22
after-lambda19
  (restore env)
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
  (restore continue)
#<undef>

;;ex5.38
;;a
(define (spread-arguments operands)
  (let ((compile1 (compile (car operands) 'arg1 'next))
	(compile2 (compile (cadr operands) 'arg2 'next)))
    (list compile1 compile2)))

;;b
(define (compile exp target linkage)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp) (compile-variable exp target linkage))
	((assignment? exp) (compile-assignment exp target linkage))
	((definition? exp) (compile-definition exp target linkage))
	((if? exp) (compile-if exp target linkage))
	((lambda? exp) (compile-lambda exp target linkage))
	((begin? exp) (compile-sequence exp target linkage))
	((cond? exp) (compile (cond->if exp) target linkage))
	((memq (operator exp) '(= * - +)) (compile-opencode exp target linkage)) ;;add
	((application? exp) (compile-application exp target linkage))
	(else (error "unknown expression type: COMPILE" exp))))

(define (compile-opencode exp target linkage)
  (if (= (length exp) 3)
      (let ((proc (operator exp))
	    (args (spread-arguments (operands exp))))
	(end-with-linkage linkage
			  (append-instruction-sequences
			   (car args)
			   (preserving '(arg1)
				       (cadr args)
				       (make-instruction-sequence '(arg1 arg2)
								  (list target)
								  `((assign ,target (op ,proc) (reg arg1) (reg arg2))))))))
      (error "Oepn code must have 2 operands: COMPILE")))
      
;;c
;;open code
(env)
(val)
  (assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))
entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign arg1 (op lookup-variable-value) (const n) (reg env))
  (assign arg2 (const 1))
  (assign val (op =) (reg arg1) (reg arg2))
  (test (op false?) (reg val))
  (branch (label false-branch4))
true-branch3
  (assign val (const 1))
  (goto (reg continue))
false-branch4
  (save continue)
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (assign arg1 (op lookup-variable-value) (const n) (reg env))
  (assign arg2 (const 1))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
compiled-branch7
  (assign continue (label proc-return9))
  (assign val (op compiled-prcedure-entry) (reg proc))
  (goto (reg val))
proc-return9
  (assign arg1 (reg val))
  (goto (reg val))
primitive-branch6
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
after-call8
  (assign arg2 (op lookup-variable-value) (const n) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
after-if5
after-lambda2
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
#<undef>

;;ex5.45
;;a

;;compile-and-go
;;total-pushes: 6*n+1
;;maximum-depth: 3*n-1

;;eceval
;;total-pushes: 32*n-16
;;maximum-depth: 5*n+3

;;手書き版
;;;;total-pushes: 2*n-2
;;maximum-depth: 2*n-2

;;b
;;factorialは基本手続きのみなので、オープンコードによる最適化が有効であると考えられる。

;;ex4.46
;;maximum-depth: 3*n-1

;;total-pushes
;;n=3 27
;;n=4 47
;;n=5 77 = S(4)+S(3)+3
;;n=6 127 = S(5)+S(4)+3
;;n=7 207 = S(6)+S(5)+3

;;k=3

;;S(3)(=27)=a*fib(4)(=3)+b
;;S(4)(=47)=a*fib(5)(=5)+b
;;S(5)(=77)=a*fib(6)(=8)+b
;;a=10 b=-3

;;ex5.47
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(compound-branch (make-label 'compound-branch))
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage
	   (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
				  `((test (op primitive-procedure?) (reg proc))
				    (branch (label ,primitive-branch))))
       (make-instruction-sequence '(proc) '()
				  `((test (op compiled-procedure?) (reg proc))
				    (branch (label ,compiled-branch))))
       (parallel-instruction-sequences
	(append-instruction-sequences
	 compound-branch
	 (compound-proc-appl target compiled-linkage))
	(parallel-instruction-sequences
	 (append-instruction-sequences
	  compiled-branch
	  (compile-proc-appl target compiled-linkage))
	 (append-instruction-sequences
	  primitive-branch
	  (end-with-linkage linkage
			    (make-instruction-sequence '(proc argl)
						       (list target)
						       `((assign ,target
								 (op apply-primitive-procedure)
								 (reg proc)
								 (reg argl))))))))
       after-call))))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
	 (make-instruction-sequence '() all-regs
				    `((assign continue (label ,linkage))
				      (save continue)
				      (goto (reg compapp)))))
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence '(proc) all-regs
				      `((assign continue (label ,proc-return))
					(save continue)
					(goto (reg compapp))
					,proc-return
					(assign ,target (reg val))
					(goto (label ,linkage))))))
	((and (eq? target 'val) (eq? linkage 'return))
	 (make-instruction-sequence
	  '(proc continue)
	  all-regs
	  '((save continue)
	    (goto (reg compapp)))))
	((and (not (eq? target 'val))
	      (eq? linkage 'return))
	 (error "return linkage, target not val: COMPILE"
		target))))

(define explicit-control-evaluator
  (make-machine
   '(exp env val continue proc argl unev compapp)
   ecval-operations
   '((assign compapp (label compound-apply))
     (branch (label external-entry)) ;;add
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     ev-appl-did-operator
     (restore unev) ;; operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))
     apply-dispatch 
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (test (op compiled-procedure?) (reg proc)) ;;add
     (branch (label compiled-apply)) ;;add
     (goto (label unknown-procedure-type))
     compiled-apply ;;add
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc)) ;;add
     (goto (reg val)) ;;add
     primitive-apply
     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
     (restore continue)
     (goto (reg continue))
     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))
     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))
     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))
;;;     ev-sequence
;;;     (test (op no-more-exps?) (reg unev))
;;;     (branch (label ev-sequence-end))
;;;     (assign exp (op first-exp) (reg unev))
;;;     (save unev)
;;;     (save env)
;;;     (assign continue (label ev-sequence-continue))
;;;     (goto (label eval-dispatch))
;;;     ev-sequence-continue
;;;     (restore env)
;;;     (restore unev)
;;;     (assign unev (op rest-exps) (reg unev))
;;;     (goto (label ev-sequence))
;;;     ev-sequence-end
;;;     (restore continue)
;;;     (goto (reg continue))
     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))
     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type
     (restore continue)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     external-entry ;;add
     (perform (op initialize-stack)) ;;add
     (assign env (op get-global-environment)) ;;add
     (assign continue (label print-result)) ;;add
     (goto (reg val)) ;;add
     )))

;;ex5.48
(define (compile-and-run expression)
  (let ((instructions
	 (assemble
	  (statements
	   (compile expression 'val 'return))
	  explicit-control-evaluator)))
    (set-register-contents! explicit-control-evaluator 'val instructions)
    (set-register-contents! explicit-control-evaluator 'flag 'true)
    (start explicit-control-evaluator)))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list 'list list)
	(list 'memq memq)
	(list 'member member)
	(list 'assoc assoc)
	(list 'not not)
	(list '= =)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '> >)
	(list '< <)
	(list 'abs abs)
	(list 'remainder remainder)
	(list 'print print)
	(list 'newline newline)
	(list 'display display)
	(list 'eq? eq?)
	(list 'compile-and-run compile-and-run) ;;add
	;; more primitives...
	))

;;ex5.49
(define (make-repl-machine) repl-machine)
(define operations
  (list
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   (list 'true? true?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'read read)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)
   (list 'get-global-environment get-global-environment)
   (list 'make-compiled-procedure make-compiled-procedure) ;;add
   (list 'compiled-procedure? compiled-procedure?) ;;add
   (list 'compiled-procedure-entry compiled-procedure-entry) ;;add
   (list 'compiled-procedure-env compiled-procedure-env) ;;add
   (list 'list list)
   (list 'cons cons)
   (list 'false? false?)
   (list 'compile compile)
   (list 'assemble assemble)
   (list 'statements statements)
   (list 'make-repl-machine make-repl-machine)
   ))

(define repl-machine
  (make-machine
   '(exp env val proc argl continue unev compapp machine)
   operations
   '((assign machine (op make-repl-machine))
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;; REPL input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label compile))
     print-result
     (perform (op announce-output) (const ";;; REPL value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     compile
     (assign exp (op compile) (reg exp) (const val) (const return))
     (assign exp (op statements) (reg exp))
     (goto (label assemble))
     assemble
     (assign val (op assemble) (reg exp) (reg machine))
     (goto (reg val)))
   ))

(define (start-repl-machine)
  (set! the-global-environment (setup-environment))
  (set-register-contents! repl-machine 'flag 'false)
  (start repl-machine))
