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
