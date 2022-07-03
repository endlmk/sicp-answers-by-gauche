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
