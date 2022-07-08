(define (compile exp target linkage ct-env) ;;add
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp) (compile-variable exp target linkage ct-env)) ;;add
	((assignment? exp) (compile-assignment exp target linkage ct-env)) ;;add
	((definition? exp) (compile-definition exp target linkage ct-env)) ;;add
	((if? exp) (compile-if exp target linkage ct-env)) ;;add
	((lambda? exp) (compile-lambda exp target linkage ct-env)) ;;add
	((begin? exp) (compile-sequence exp target linkage ct-env)) ;;add 
	((cond? exp) (compile (cond->if exp) target linkage ct-env)) ;;add
	((application? exp) (compile-application exp target linkage ct-env)) ;;add
	(else (error "unknown expression type: COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;5.2.2
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return) (make-instruction-sequence '(continue) '() '((goto (reg continue)))))
	((eq? linkage 'next) (empty-instruction-sequence))
	(else (make-instruction-sequence '() '() `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue) instruction-sequence (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '() (list target)
					       `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '() (list target)
					       `((assign ,target (const ,(text-of-quotation exp)))))))
;;ex5.42
(define (compile-variable exp target linkage ct-env)
  (let ((lexical-address (find-variable exp ct-env)))
    (end-with-linkage linkage
		      (make-instruction-sequence '(env) (list target)
						 (if (eq? lexical-address 'not-found)
						     `((assign ,target
							       (op lookup-variable-value)
							       (const ,exp)
							       (reg env)))
						     `((assign ,target
							       (op lexical-address-lookup)
							       (const ,lexical-address)
							       (reg env))))))))
;;ex5.42
(define (compile-assignment exp target linkage ct-env) ;;add
  (let ((var (assignment-variable exp))
	(get-value-code (compile (assignment-value exp) 'val 'next ct-env))) ;;add
    (let ((lexical-address (find-variable var ct-env)))
      (end-with-linkage linkage
			(preserving '(env)
				    get-value-code
				    (make-instruction-sequence '(env val) (list target)
							       (if (eq? lexical-address 'not-found)
								   `((perform (op set-variable-value!)
									      (const ,var)
									      (reg val)
									      (reg env))
								     (assign ,target (const ok)))
								   `((perform (op lexical-address-set!)
									      (const ,lexical-address)
									      (reg val)
									      (reg env))
								     (assign ,target (const ok))))))))))
(define (compile-definition exp target linkage ct-env) ;;add
  (let ((var (definition-variable exp))
	(get-value-code (compile (definition-value exp) 'val 'next ct-env))) ;;add
    (end-with-linkage linkage
		      (preserving '(env)
				  get-value-code
				  (make-instruction-sequence '(env val) (list target)
							     `((perform (op define-variable!)
									(const ,var)
									(reg val)
									(reg env))
							       (assign ,target (const ok))))))))
(define (compile-if exp target linkage ct-env) ;;add
  (let ((t-branch (make-label 'true-branch))
	(f-branch (make-label 'false-branch))
	(after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next ct-env)) ;;add
	    (c-code (compile (if-consequent exp) target consequent-linkage ct-env)) ;;add
	    (a-code (compile (if-alternative exp) target linkage ct-env))) ;;add
	(preserving '(env continue)
		    p-code
		    (append-instruction-sequences
		     (make-instruction-sequence '(val) '()
						`((test (op false?) (reg val))
						  (branch (label ,f-branch))))
		     (parallel-instruction-sequences
		      (append-instruction-sequences t-branch c-code)
		      (append-instruction-sequences f-branch a-code))
		     after-if))))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
		  (number->string (new-label-number)))))

(define (compile-sequence seq target linkage ct-env) ;;add
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage ct-env) ;;add
      (preserving '(env continue)
		  (compile (first-exp seq) target 'next ct-env) ;;add
		  (compile-sequence (rest-exps seq) target linkage ct-env))))

(define (compile-lambda exp target linkage ct-env) ;;add
  (let ((proc-entry (make-label 'entry))
	(after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
	   (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
	(end-with-linkage lambda-linkage
			  (make-instruction-sequence '(env) (list target)
						     `((assign ,target
							       (op make-compiled-procedure)
							       (label ,proc-entry)
							       (reg env)))))
	(compile-lambda-body exp proc-entry ct-env)) ;;add
       after-lambda))))

(define (compile-lambda-body exp proc-entry ct-env) ;;add
  (let ((formulas (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
				`(,proc-entry
				  (assign env
					  (op compiled-procedure-env)
					  (reg proc))
				  (assign env
					  (op extend-environment)
					  (const ,formulas)
					  (reg argl)
					  (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return (cons formulas ct-env))))) ;;ex5.40
	
;;5.5.3
(define (compile-application exp target linkage ct-env) ;;add
  (let ((proc-code (compile (operator exp) 'proc 'next ct-env)) ;;add
	(operand-codes
	 (map (lambda
		  (operand) (compile operand 'val 'next ct-env)) ;;add
	      (operands exp))))
    (preserving '(env continue)
		proc-code
		(preserving '(proc continue)
			    (construct-arglist operand-codes)
			    (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
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
			   (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
	 (preserving '(argl)
		     (car operand-codes)
		     (make-instruction-sequence '(val argl) '(argl)
						'((assign argl
							  (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
	code-for-next-arg
	(preserving '(env)
		    code-for-next-arg
		    (code-to-get-rest-args (cdr operand-codes))))))
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage
	   (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
				  `((test (op primitive-procedure?) (reg proc))
				    (branch (label ,primitive-branch))))
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
								(reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
	 (make-instruction-sequence '(proc) all-regs
				    `((assign continue (label ,linkage))
				      (assign val (op compiled-procedure-entry)
					      (reg proc))
				      (goto (reg val)))))
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence '(proc) all-regs
				      `((assign continue (label ,proc-return))
					(assign val (op compiled-prcedure-entry)
						(reg proc))
					(goto (reg val))
					,proc-return
					(assign ,target (reg val))
					(goto (reg val))))))
	((and (eq? target 'val) (eq? linkage 'return))
	 (make-instruction-sequence
	  '(proc continue)
	  all-regs
	  '((assign val (op compiled-procedure-entry)
		    (reg proc))
	    (goto (reg val)))))
	((and (not (eq? target 'val))
	      (eq? linkage 'return))
	 (error "return linkage, target not val: COMPILE"
		target))))

(define all-regs '(env proc val argl continue))
	      
;;5.5.4
(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
		       (registers-modified seq1)))
     (list-union (registers-modified seq1)
		 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
	(empty-instruction-sequence)
	(append-2-sequences
	 (car seqs)
	 (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
	((memq (car s1) s2) (list-union (cdr s1) s2))
	(else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
	((memq (car s1) s2) (list-difference (cdr s1) s2))
	(else (cons (car s1)
		    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
	(if (and (needs-register? seq2 first-reg)
		 (modifies-register? seq1 first-reg))
	    (preserving (cdr regs)
			(make-instruction-sequence
			 (list-union (list first-reg)
				     (registers-needed seq1))
			 (list-difference (registers-modified seq1)
					  (list first-reg))
			 (append `((save ,first-reg))
				 (statements seq1)
				 `((restore ,first-reg))))
			seq2)
	    (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
	   (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
	       (registers-needed seq2))
   (list-union (registers-modified seq1)
	       (registers-modified seq2))
   (append (statements seq1)
	   (statements seq2))))
					  
(define (parse-compiled-code lis)
  (if (not (null? lis))
      (begin
        (if (pair? (caar lis))
            (map (lambda (x)
                         (if (symbol? x)
                             (print x)
                             (print "  " x)))
                 (car lis))
            (print (car lis)))
        (parse-compiled-code (cdr lis)))))	
		      
;;ex5.39
(define (lexical-address-lookup lexical-address env)
  (let ((frame-number (car lexical-address))
	(displacement-number (cadr lexical-address)))
    (list-ref (list-ref env frame-number) displacement-number)))

;;ex5.41
(define (find-variable var ct-env)
  (define (frame-iter frame-number frames)
    (if (null? frames)
	'not-found
	(let ((displacement-number (scan-iter 0 (car frames))))
	  (if displacement-number
	      (list frame-number displacement-number)
	      (frame-iter (+ frame-number 1) (cdr frames))))))
  (define (scan-iter displacement-number frame)
    (if (null? frame)
	#f
	(if (eq? var (car frame))
	    displacement-number
	    (scan-iter (+ displacement-number 1) (cdr frame)))))
  (frame-iter 0 ct-env))
	      
;;ex5.42
(define (lexical-address-set! lexical-address val env)
  (let ((var (lexical-address-lookup lexical-address env)))
    (set! var val)))
	
;;ex5.43
(define (compile-lambda-body exp proc-entry ct-env) ;;add
  (let ((formulas (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
				`(,proc-entry
				  (assign env
					  (op compiled-procedure-env)
					  (reg proc))
				  (assign env
					  (op extend-environment)
					  (const ,formulas)
					  (reg argl)
					  (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return (cons formulas ct-env))))) ;;ex5.40

;;add let
(define (compile exp target linkage ct-env) ;;add
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp) (compile-variable exp target linkage ct-env)) ;;add
	((assignment? exp) (compile-assignment exp target linkage ct-env)) ;;add
	((definition? exp) (compile-definition exp target linkage ct-env)) ;;add
	((if? exp) (compile-if exp target linkage ct-env)) ;;add
	((lambda? exp) (compile-lambda exp target linkage ct-env)) ;;add
	((let? exp) (compile (let->combination exp) target linkage ct-env)) ;; add let 
	((begin? exp) (compile-sequence exp target linkage ct-env)) ;;add 
	((cond? exp) (compile (cond->if exp) target linkage ct-env)) ;;add
	((application? exp) (compile-application exp target linkage ct-env)) ;;add
	(else (error "unknown expression type: COMPILE" exp))))

;;ex5.44
(define (compile exp target linkage ct-env) ;;add
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp) (compile-variable exp target linkage ct-env)) ;;add
	((assignment? exp) (compile-assignment exp target linkage ct-env)) ;;add
	((definition? exp) (compile-definition exp target linkage ct-env)) ;;add
	((if? exp) (compile-if exp target linkage ct-env)) ;;add
	((lambda? exp) (compile-lambda exp target linkage ct-env)) ;;add
	((let? exp) (compile (let->combination exp) target linkage ct-env)) ;; add let 
	((begin? exp) (compile-sequence exp target linkage ct-env)) ;;add 
	((cond? exp) (compile (cond->if exp) target linkage ct-env)) ;;add
	((opencode? exp ct-env) (compile-opencode exp target linkage ct-env)) ;;add	
	((application? exp) (compile-application exp target linkage ct-env)) ;;add
	(else (error "unknown expression type: COMPILE" exp))))

(define (opencode? exp ct-env)
  (let ((op (operator exp)))
    (if (eq? (find-variable op ct-env) 'not-found)
	#f
	(memq op '(= * - +)))))

(define (spread-arguments operands ct-env) ;;add 
  (let ((compile1 (compile (car operands) 'arg1 'next ct-env)) ;;add
	(compile2 (compile (cadr operands) 'arg2 'next ct-env))) ;;add
    (list compile1 compile2)))
	 
(define (compile-opencode exp target linkage ct-env) ;;add
  (if (= (length exp) 3)
      (let ((proc (operator exp))
	    (args (spread-arguments (operands exp) ct-env))) ;;add
	(end-with-linkage linkage
			  (append-instruction-sequences
			   (car args)
			   (preserving '(arg1)
				       (cadr args)
				       (make-instruction-sequence '(arg1 arg2)
								  (list target)
								  `((assign ,target (op ,proc) (reg arg1) (reg arg2))))))))
      (error "Oepn code must have 2 operands: COMPILE")))
