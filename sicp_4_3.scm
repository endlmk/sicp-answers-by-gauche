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
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))

;;ex4.37
;;Benの実装のほうが効率的である
;;low:1 high2の場合、
;;Ben
;;1 2
;;|\|
;;122
;;4.53、ループが3重になり、探索範囲が大きくなる程Benの実装より効率が悪くなる
;;1  2
;;|\ |
;;12 2
;;|\||
;;1222

(define (require p) (if (not p) (amb)))
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
(define (an-integer-between l h)
  (require (> h l))
  (amb l (an-integer-between (+ l 1) h)))