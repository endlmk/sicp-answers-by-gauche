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
