;;ex4.35
(define (an-integer-between l h)
  (require (<= l h))
  (amb l (an-integer-between (+ l 1) h)))

;;ex4.36
;;kに上限がないとバックトラックしないため、kが無限に増え続ける
;;長辺kを決めて、他の二辺の和がkを超えないようにする
(define (a-pytagorean-triple)
  (let ((k (an-integer-starting-from 2)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
	(require (= (+ (* i i) (* j j) (* k k))))
	(list i j k)))))
