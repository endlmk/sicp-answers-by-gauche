;;ex4.25
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
;;適用順序の場合、unlessのusual,exceptionalどちらも評価を終える必要があるが、usualでfactorialを再帰させているため、無限に再帰する。
;;正規順序の場合、n=1の場合にunlessの評価が完了し、正しく動作する。

;;ex4.26
(define (unless->if exp)
  (let ((pproc (cadr exp))
	(cproc (cadddr exp))
	(aproc (caddr exp)))
    (make-if pproc cproc aproc)))
;;手続きだとmapのような高階関数に渡すことができる
(map unless '(#t #t #f #f) '(1 2 3 4) '(5 6 7 8)) 

;;ex4.27
;;; L-Eval input:
(define count 0)
;;; L-Eval input:
(define (id x) (set! count (+ count 1)) x)
;;; L-Eval input:
(define w (id (id 10))) ;;<-この時点では外側のidを評価した際にset!が呼ばれる。内側のidはサンクとして環境に配置され、未評価の状態である。
;;; L-Eval input:
count
;;; L-Eval value:
1 ;;<-上述の通り、set!が一度だけ呼ばれるためcountは1となる
;;; L-Eval input:
w ;;<-この際にdriver-loopのactual-valueでサンクである内側のidがforce-itされることでset!が呼ばれる
;;; L-Eval value:
10
;;; L-Eval input:
count
;;; L-Eval value:
1 ;;<-上述の通り、wを表示した際にset!が呼ばれるためcountは2となる

;;ex4.28
;;引数として手続きを渡し、その手続きが適用される際、actual-valueで実際の値を取得しないと、applyでエラーとなる
;;define (evalL exp env)
;;	  ((application? exp) (apply (operator exp) (operands exp) env))
;;とすると、以下の2つ目でエラーとなる
(define (test f) (f 3))
(test (lambda (x) (+ x 2)))

;;ex4.29
(define (rep x n)
  (if (= n 0)
      ()
      (cons x (rep x (- n 1)))))
(define (square x) (* x x))
(rep (square 10) 10)
;;メモ化がない場合、サンクとして渡された(square 10)が再帰のたびに評価される

(define (square x) (* x x))
(square (id 10))
100
count
1 ;;メモ化がある場合、サンクを評価した際にset!が呼び出され、その後はメモ化されたサンクとなるため、set!が呼ばれない。よって、countは1となる。
2 ;;メモ化がない場合、サンクを評価するたびにset!が呼ばれる。squareで二回サンクが評価されるため、countは2となる。

;;ex4.30
;;a
;;for-eachが作るbeginの各要素はfor-eachの第一引数の手続きを適用する。
;;この引数のcarは基本手続きなので、遅延されない。
;;また、この手続きの中のnewline,dislayも基本手続きなので、遅延されない。
;;よって全てのリストの要素に対してdisplayが呼ばれる。
;;b
;;original
;;; L-Eval input:
(p1 1)

;;; L-Eval value:
(1 2)

;;; L-Eval input:
(p2 1)

;;; L-Eval value:
1

;;Cy
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evalL (first-exp exps) env))
	(else (actual-value (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))
;;; L-Eval input:
(p1 1)

;;; L-Eval value:
(1 2)

;;; L-Eval input:
(p2 1)

;;; L-Eval value:
(1 2)

;;c
(define (evalL exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
	((begin? exp) (eval-sequence (begin-actions exp) env))
	((cond? exp) (evalL (cond->if exp) env))
	((application? exp) (apply #?=(actual-value (operator exp) env) (operands exp) env))
	(else (error "Unknown expression type: EVAL" exp))))
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
	     (for-each proc (cdr items)))))
(for-each (lambda (x) (newline) (display x))
	  '(57 321 88))
;;actual-valueはevalの後にforce-itを呼び、force-itは引数がサンクでない場合はそのまま引数を返すだけなので単にevalを呼ぶのと結果が変わらないため。

;;d
;;どちらにも難点がある。
;;本文中の方式はCyの主張の通り、列中の最後のもの以外が使われない場合強制されない。
;;Cyの方式は、列中は値は強制され、遅延されないことになる。

;;ex4.31
;;See http://sioramen.sub.jp/blog/2008/02/sicp-422.html
;;procedureを作る際に、仮引数とペアになるlazyとlazy-memoを覚えておく。
;;applyにて、仮引数に引数を束縛する際に、procedureに覚えていたlazy,lazy-memo, ()(遅延なし、ペアでない仮引数)を使って、遅延させるか、メモ化ありの遅延をさせるかを分岐する。

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items)) (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
	((null? list2) list1)
	(else (cons (+ (car list1) (car list2))
		    (add-lists (cdr list1) (cdr list2))))))
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
	  (add-lists (scale-list integrand dt) int)))
  int)
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)
(list-ref (solve (lambda (x) x) 1 0.001) 1000)
;;ex4.32
;;consの第一引数が遅延される。そのため、以下のような未定義のxのストリームが作れる。
(define xs (cons x xs)))
