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

;;b
(define count-leaves-iter-machine
  (make-machine
   '(tree val n tmp continue)
   (list (list '+ +) (list 'car car) (list 'cdr cdr) (list 'null? null?) (list 'pair? pair?) (list 'not not))
   '(controller
     (assign continue (label done))
     (assign n (const 0))
     loop
     (test (op null?) (reg tree))
     (branch (label base1))
     (assign tmp (op pair?) (reg tree))
     (test (op not) (reg tmp))
     (branch (label base2))
     (save continue)
     (assign continue (label iter))
     (save tree)
     (assign tree (op cdr) (reg tree))
     (goto (label loop))
     iter
     (restore tree)
     (restore continue)
     (assign tree (op car) (reg tree))
     (assign n (reg val))
     (goto (label loop))
     base1
     (assign val (reg n))
     (goto (reg continue))
     base2
     (assign val (op +) (reg n) (const 1))
     (goto (reg continue))
     done))))

(define x (cons (list 1 2) (list 3 4)))
(set-register-contents! count-leaves-iter-machine 'tree (list x x))
(start count-leaves-iter-machine)
(get-register-contents count-leaves-iter-machine 'val)    
	     
;;ex5.22
(define append-machine
  (make-machine
   '(x y val tmp continue)
   (list (list 'null? null?) (list 'cons cons) (list 'car car) (list 'cdr cdr))
   '(controller
     (assign continue (label done))
     loop
     (test (op null?) (reg x))
     (branch (label base))
     (save continue)
     (assign continue (label rec))
     (save x)
     (assign x (op cdr) (reg x))
     (goto (label loop))
     rec
     (restore x)
     (restore continue)
     (assign tmp (op car) (reg x))
     (assign val (op cons) (reg tmp) (reg val))
     (goto (reg continue))
     base
     (assign val (reg y))
     (goto (reg continue))
     done)))
(set-register-contents! append-machine 'x '(1 2 3))
(set-register-contents! append-machine 'y '(4 5 6))
(start append-machine)
(get-register-contents append-machine 'val)

(define append!-machine
  (make-machine
   '(x y tmp)
   (list (list 'null? null?) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
   '(controller
     (save x)
     loop
     (assign tmp (op cdr) (reg x))
     (test (op null?) (reg tmp))
     (branch (label lastfound))
     (assign x (op cdr) (reg x))
     (goto (label loop))
     lastfound
     (assign x (op set-cdr!) (reg x) (reg y))
     done
     (restore x))))
(set-register-contents! append!-machine 'x (list 1 2 3)) ;; '()形式のリストはimmutableなのでset-cdr!できない!
(set-register-contents! append!-machine 'y (list 4 5 6))
(start append!-machine)
(get-register-contents append!-machine 'x)


