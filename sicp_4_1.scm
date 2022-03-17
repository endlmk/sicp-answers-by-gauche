(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (look-up-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
	((begin? exp) (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval-and exp env))
	((or? exp) (eval-or exp env))
	((let*? exp) (eval (let*->nested-lets exp) env))
	((let? exp) (eval (let->combination exp) env))
	((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
	(else (error "Unknown expression type: EVAL" exp))))
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure) (eval-sequence (procedure-body procedure)
							(extend-environment
							 (procedure-parameters procedure)
							 arguments
							 (procedure-environment procedure))))
	(else (error "Unknown procedure type: APPLY" procedure))))
(define (list-of-values exps env)
  (if (no-operands? exps)
      ()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;ex4.1
(define (list-of-values exps env)
  (if (no-operands? exps)
      ()
      (let ((l (eval (first-operand exps) env))
	    (r (list-of-values (rest-operands exps) env)))
	(cons l r))))    
(define (list-of-values exps env)
  (if (no-operands? exps)
      ()
      (let ((l (eval (first-operand exps) env))
	    (r (list-of-values (rest-operands exps) env)))
	(cons r l))))    

;; self evaluating -> number, string
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))
;; variable -> symbol
(define (variable? exp) (symbol? exp))
;; quoted -> (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;; assignment -> (set! <variable> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
;; define -> (define <var> <value>) or (define (<var> <param1> ... <paramn>) <body>)
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;; params
		   (cddr exp)))) ;; body
;; lambda -> (lambda ...)
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons (parameters body))))
;; if -> (if (<pred>) (<consequent>) ((<alternative>)/null))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))
;; seq->exp
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
;; etc
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;; cond->if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clauses? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;;ex4.2
;;a pairで表現する関数適用以外の構文が全て関数適用として処理されてしまう。
;;b 以下の処理に置き換え、application?をevalの先頭に持っていく。
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;;ex4.3
(define *op-table* (make-hash-table 'equal?))
(define (put op type proc)
  (hash-table-put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table-get *op-table* (list op type) #f))
(define (eval-data-driven exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (look-up-value exp env))
	(else (let ((proc (get 'eval-data-driven (get-tag exp))))
		(if proc
		    (proc exp env)
		    (apply (eval (operator exp) env) (list-of-values (operands exp) env)))))))
(define (get-tag exp) (car exp))  
;;install dispatch
(define (install-quote)
  (put 'eval-data-driven 'quote (lambda (exp env) (text-of-quotation exp))))
(define (install-assignment)
  (put 'eval-data-driven 'set! eval-assignment))
(define (install-definition)
  (put 'eval-data-driven 'define eval-definition))
(define (install-if)
  (put 'eval-data-driven 'if eval-if))
(define (install-lambda)
  (put 'eval-data-driven 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env))))
(define (install-begin)
  (put 'eval-data-driven 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env))))
(define (install-cond)
  (put 'eval-data-driven 'cond (lambda (exp env) (eval (cond->if exp) env))))

;;ex4.4
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (eval-and exp env)
  (define (iter operands env)
    (if (null? operands)
	#t
	(if (eval (car operands) env)
	    (iter (cdr operands) env)
	    #f)))
  (iter (operands exp) env))
(define (eval-or exp env)
  (define (iter operands env)
    (if (null? operands)
	#f
	(if (eval (car operands) env)
	    #t
	    (iter (cdr operands) env))))
  (iter (operands exp) env))
;;expand version
(define (and->if exp)
  (define (expand-and operands)
    (if (null? operands)
	#t
	(make-if (car operands)
		 (expand-and (cdr operands))
		 #f)))
  (expand-and (operands exp)))
(define (or->if exp)
  (define (expand-or operands)
    (if (null? operands)
	#f
	(make-if (car operands)
		 #t
		 (expand-or (cdr operands)))))
  (expand-or (operands exp)))
;;evalの分岐に以下を追加する
((and? exp) (eval (and->if exp) env))
((or? exp) (eval (or->if exp) env))

;;ex4.5
(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clauses? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (if (eq? (car (cond-actions first)) '=>) ;; add
			 (list (cadr (cond-actions first)) (cond-predicate first)) ;;add
			 (sequence->exp (cond-actions first)))
		     (expand-clauses rest))))))

;;ex4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-var-exp exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-vars var-exp) (map car var-exp))
(define (let-exps var-exp) (map cadr var-exp))
(define (let->cobination exp)
  (let ((vars (let-vars (let-var-exp exp)))
	(exps (let-exps (let-var-exp exp)))
	(body (let-body exp)))
    (cons (make-lambda vars body) exps)))

;;ex4.7
;;(let* ((var1 exp1) (var2 exp2) ... (varn expn))
;;  (body))
;;->
;;(let ((var1 exp1))
;;  (let ((var2 exp2))
;;   ...
;;      (let ((varn expn))
;;          (body))...))
(define (let*->nested->let exp)
  (define (iter var-exp body)
    (if (null? vars)
	body
	(list 'let (list (car var-exp)) (iter (cdr var-exp body)))))
  (let ((var-exp (let-var-exp exp))
	(body (let-body exp)))
    (iter var-exp body)))

;;letを再帰的に処理可能なので、派生式として定義可能である

;;ex4.8
(define (let? exp) (tagged-list? exp 'let))
(define (let-has-proc? exp) (not (pair? (cadr exp))))
(define (let-proc exp) (if (let-has-proc? exp) (cadr exp) #f))
(define (let-var-exp exp) (if (let-has-proc? exp) (caddr exp) (cadr exp)))
(define (let-body exp) (if (let-has-proc? exp) (cadddr exp) (caddr exp)))
(define (let-vars var-exp) (map car var-exp))
(define (let-exps var-exp) (map cadr var-exp))
(define (let->cobination exp)
  (let ((proc (let-proc exp))
	(vars (let-vars (let-var-exp exp)))
	(exps (let-exps (let-var-exp exp)))
	(body (let-body exp)))
    (if proc
	(cons (cons 'define (cons (cons proc vars) body)) (cons proc exps))  ;; (define (proc vars...) (body)) (proc exps...)
	(cons (make-lambda vars body) exps))))

;;ex4.9
;;(while exp body)
;;->
;;(let loop ()
;;  (if exp
;;      (begin body loop)))
(define (while? (tagged-list? 'while)))
(define (while->let exp)
  (let ((condition (cadr exp))
	(body (caddr exp)))
    (list 'let 'loop () (make-if condition (make-begin (cons body 'loop))))))

;;ex4.10
;;skip.

	      

	

