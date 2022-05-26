;;ex5.8
(controller
 start
 (goto (label here))
 here
 (assign a (const 3))
 (goto (label there))
 here
 (assign a (const 4))
 (goto (label there))
 there)
	 
;;後のhereがラベルとして有効になる。そのため、aは4になる

(define (extract-labels text recieve)
  (if (null? text)
      (recieve () ())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
	 (let ((next-inst (car text)))
	   (if (symbol? next-inst)
	       (let ((label (assoc next-inst labels)))
		 (if label
		     (error "Already defined label: ASSEMBLE" label)
		     (recieve insts
			      (cons (make-label-entry next-inst insts)
				    labels))))
		 (recieve (cons (make-instruction next-inst)
				insts)
			  labels)))))))
