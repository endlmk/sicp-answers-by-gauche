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
	((variable? exp) (lookup-value exp env))
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
(define (let-body exp) (cddr exp))
(define (let-vars var-exp) (map car var-exp))
(define (let-exps var-exp) (map cadr var-exp))
(define (let->combination exp)
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
(define (let-body exp) (if (let-has-proc? exp) (cdddr exp) (cddr exp)))
(define (let-vars var-exp) (map car var-exp))
(define (let-exps var-exp) (map cadr var-exp))
(define (let->combination exp)
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
	
;;ex4.11
(define (make-frame variables values)
  (use srfi-1)
  (zip variables values))
(define (add-binidings-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings) (env-loop (enclosing-environment env)))
	    ((eq? var (car (car bindings))) (cdr (car bindings))
	    (else (scan (cdr bindings))))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan frame))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings) (env-loop (enclosing-environment env)))
	    ((eq? var (car (car bindings))) (set-car! bindings (cons var val)))
	    (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable: SET!" var)
	(let ((frame (first-frame env)))
	  (scan frame))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings) (add-biniding-to-frame! var val frame))
	    ((eq? var (car (car bindings))) (set-car! bindings (cons var val)))
	    (else (scan (cdr bindings)))))
    (scan frame)))

;;ex4.12
(define (scan-environment var env found-var-vals-proc not-found-var-proc no-var-error-msg)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (if not-found-var-proc
			      (not-found-var-proc)
			      (env-loop (enclosing-environment env))))
	    ((eq? var (car vars)) (found-var-proc vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error no-var-error-msg var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))
(define (lookup-variable-value var env)
  (scan-environment var env car #f "Unbound variable"))
(define (set-variable-value! var val env)
  (scan-environment var env (lambda (vals) (set-car! vals val)) #f "Unbound variable: SET!"))
(define (define-varible! var val env)
  (let ((frame (first-frame env)))
    (scan-environment var env (lambda (vals) (set-car! vals val)) (lambda () (add-biniding-to-frame! var val frame)) "")))

;;ex4.13
(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (begin (set! vars (map (lambda (x) (not (eq? x (car vars)))) vars))
				   (set! vals (map (lambda (x) (not (eq? x (car vals)))) vals))
				   (env-loop (enclosing-environment env))))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Binding not found" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

;;ex4.14
;;schemeのmapを呼び出すとmapする関数をschemeの環境から探してしまうが、
;;mapする関数はメタ循環評価器の環境にあるため発見できない。

;;ex4.15
;;(halts? try try)がtrueの場合、tryは停止すると判定されているが、(run-forever)が実行され、無限に動き続けるので矛盾する。
;;(halts? try try)がfalseの場合、tryは停止しないと判定されているが、'haltedを返して停止するので矛盾する。

;;ex4.16
;;a
(define(lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (if (eq? (car vals) '*unassigned*)
				      (error "Unassigned value used" var)
				      (car vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))
;;b
(define (scan-out-defines exp)
  (let ((define-variables (map cadr (filter define? exp))))
    (cons (list 'let (map (lambda (var) (list var '*unassigned*)) define-variables))
	  (map (lambda (exp) (if (define? exp)
				 (list 'set! (cdr exp))
				 exp))
	       exp))))
;;c
;;make-procedureに組み込む。評価回数が少なくなるため。
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;;ex4.17
;;['true:#t, 'false:f ]
;;(lambda <vars>
;;  (define u <e1>)<-extend-env,add-bindings-to-frame! [[u:<e1>], ['true:#t, 'false:f ]]
;;  (define v <e2>)<-extend-env,add-bindings-to-frame! [[v:<e2>], [u:<e1>], ['true:#t, 'false:f ]]
;;  <e3>)

;;(lambda <vars>
;;  (let ((u '*unassigned*)
;;        (v '*unassigned*))
;;    (set! u <e1>)
;;    (set! v <e2>)
;;    <e3>))
;;->
;;(lambda <vars>
;;  ((lambda (u v)<-extend-env [[u:'*unassigned*, v:'*unassigned*]['true:#t, 'false:f ]]
;;     (set! u <e1>)<-extend-env,add-bindings-to-frame![[u:<e1>], [u:'*unassigned*, v:'*unassigned*]['true:#t, 'false:f ]]
;;     (set! v <e2>)<-extend-env,add-bindings-to-frame![[v:<e2>], [u:<e1>], [u:'*unassigned*, v:'*unassigned*]['true:#t, 'false:f ]]
;;     <e3>)
;;     '*unassigned*
;;     '*unassigned*)
;;評価の際には自身の環境(リストの手前)から順に変数を探すので、'*unassigned*に行き着くことはない。
;;letで束縛するのではなく、define-variable!で環境を拡張せずに自身のフレームに束縛を追加すればよい。

;;ex4.18
;;(define (solve f y0 dt)
;;  (define y (integral (delay dy) y0 dt))
;;  (define dy (stream-map f y))
;;  y)
;;(define (solve f y0 dt)
;;  (let ((y '*unassigned*) (dy '*unassigned*))
;;    (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
;;      (set! u a)
;;      (set! v b))
;;      y))
;; aの束縛は動作する。(delay dy)なので、dyが'*unassigned*でも評価されないため。
;; bを束縛する際に(stream-map f y)を評価した際、yが'*unassigned*のため、エラーとなる。
;;(define (solve f y0 dt)
;;  (let ((y '*unassigned*) (dy '*unassigned*))
;;    (set! y (integral (delay dy) y0 dt))
;;    (set! dy (stream-map f y))
;;    y))
;;動作する。dyを代入する際に、yはすでに'*unassigned*でない値が代入されているため、エラーにならない。

;;ex4.19
;;Evaの主張が最も原理的に正しいが、効率的な実装が困難とのことである。ゆえにAlyssaの主張が打倒と考えられる。
;;Evaの主張を実装するとすれば、変数の評価において未割当の変数に出会った場合、その変数の評価を後に回し、
;;その後の変数で以前未割当だった変数があれば、それを評価した後に後に回した変数を評価する。
;;ただし、変数間の依存関係がある場合、その依存関係に従った評価順序となるようにする必要がある。
;;これは変数間の依存関係グラフを構築して、依存関係を正しく解決する順序を作る処理となり、効率的には処理できない。

;;ex4.20
;;a
(define (letrec? exp) (tagged-list? 'letrec))
(define (letrec-vars exp) (map car (cadr exp)))
(define (letrec-varsexps exp) (cadr exp))
(define (letrec-body exp) (caddr exp))
(define (letrec->let exp)
  (let ((vars (letrec-vars exp))
	(varsexps (letrec-varsexps exp))
	(body (letrec-body exp)))
    (list 'let
	  (map (lambda (var) (list var '*unassinged*)) vars)
	  (append (map (lambda (varexp) (cons 'set! varexp)) varsexps) (list body)))))
;;b
;;(define (f x)
;;  (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) 
;;  	     (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
;;    (<body>))) -> [[odd?:lambda...], [even?:lambda...], [even?:'*unassigned*, odd?:'*unassigned*], [x:5],['true:#t, 'false:f ]]
;;(define (f x)
;;  (let ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) 
;;  	     (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
;;    (<body>))) , [[x:5], ['true:#t, 'false:f ]]
;; even?にlambda関数を束縛する際に評価する環境ではodd?は未定義であり、エラーとなる。

;;ex4.21
;;a
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)
;;b
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))
