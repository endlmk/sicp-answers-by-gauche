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
(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

;;ex4.38
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ;;(require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))
;;答えは以下の5つ
((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
　
;;ex4.39
;;制約の順序は解に影響しない。解の候補に対し、制約の順序によらず全ての制約を満たすことがチェックされるため。
;;失敗する場合が多い制約を先にするほうが速い。そうでないと、深さ優先で探索して、より後の制約で失敗することが多くなりバックトラックが多くなるため。
;;すなわち以下の順序。
;;(require (distinct? (list baker cooper fletcher miller smith))) ;;3005 = 5^5 - 5*4*3*2*1
;;(require (> miller cooper)) ;;1875 =  5^3*(1+2+3+4+5)
;;(require (not (= (abs (- smith fletcher)) 1))) ;;1000 = 5^3*(1+2+2+2+1)
;;(require (not (= (abs (- fletcher cooper)) 1))) ;;1000 = 5^3*(1+2+2+2+1)
;;(require (not (= baker 5))) ;;625 = 5^4
;;(require (not (= cooper 1))) ;;625 = 5^4
;;(require (not (= fletcher 5))) ;;625 = 5^4
;;(require (not (= fletcher 1))) ;;625 = 5^4
