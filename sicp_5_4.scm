;;ex5.23
;;add eval-dispatch
(test (op cond?) (reg exp))
(branch (label ev-cond))
(test (op let?) (reg exp))
(branch (label ev-let))

ev-cond
(assign exp (op cond->if) (reg exp))
(goto (label eval-dispatch))
ev-let
(assign exp (op let->combination) (reg exp))
(goto (label eval-dispatch))

;;ex5.24
ev-cond
(assign unev (op cond-clauses) (reg exp))
(save continue)
ev-cond-loop
(test (op null?) (reg unev))
(branch (label ev-cond-null))
(assign exp (op car) (reg unev))
(test (op cond-else-clause?) (reg exp))
(branch (label ev-cond-action))
(save exp)
(assign exp (op cond-predicate) (reg exp))
(save unev)
(save env)
(assign continue (label ev-cond-decide))
(goto (label eval-dispatch))
ev-cond-decide
(restore env)
(restore unev)
(restore exp)
(test (op true?) (reg val))
(branch (label ev-cond-action))
(assign unev (op cdr) (reg unev))
(goto (label ev-cond-loop))
ev-cond-null
(restore continue)
(assign val (const #f))
(goto (reg continue))		
ec-cond-action
(assign unev (op cond-action) (reg exp))
(goto (label ev-sequence))
