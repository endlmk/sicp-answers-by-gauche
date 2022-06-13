;;ex5.20
;;x:p1->[*|*]->[2]
;;       |
;;      [1]
;;y:p3-> [*|*]->p2[*|/]
;;        |        |
;;        p1       p1
;; the-cars[n1|p1]
;; the-cdrs[n2|p2]
;; free:p4

;;ex5.21
;;a
(define count-leaves-machine
  (make-machine
   '(tree val result continue)
   (list (list '+ +) (list 'car car) (list 'cdr cdr) (list 'pair? pair?) (list 'null? null?) (list 'not not))
   '(controller
     (assign continue (label done))
     (assign val (const 0))
     loop
     (test (op null?) (reg tree))
     (branch (label base1))
     (assign result (op pair?) (reg tree))
     (test (op not) (reg result))
     (branch (label base2))
     (save continue)
     (assign continue (label count-car))
     (save tree)
     (assign tree (op car) (reg tree))
     (goto (label loop))
     count-car
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label count-cdr))
     (save val)
     (goto (label loop))
     count-cdr
     (assign result (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg result))
     (goto (reg continue))
     base1
     (assign val (const 0))
     (goto (reg continue))
     base2
     (assign val (const 1))
     (goto (reg continue))
     done)))

(define x (cons (list 1 2) (list 3 4)))
(set-register-contents! count-leaves-machine 'tree (list x x))
(start count-leaves-machine)
(get-register-contents count-leaves-machine 'val)
