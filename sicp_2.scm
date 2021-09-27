;;ex2-1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n_r (/ n g))
	  (d_r (/ d g)))
      (if (< d_r 0)
	  (cons (* -1 n_r) (* -1 d_r))
	  (cons n_r d_r)))))
