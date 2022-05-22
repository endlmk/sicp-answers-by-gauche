;;ex5.1
;;skip

;;ex5.2
(controller
 test-counter
 (test (op >) (reg counter) (reg n))
 (barnch (label factorial-done))
 (assign product (op *) (reg product) (reg counter))
 (assign counter (op +) (reg counter) (const 1))
 (goto (label test-counter))
 factorial-done)
       
