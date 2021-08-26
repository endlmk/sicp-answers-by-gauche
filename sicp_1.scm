;ex-1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;;ex-1.2
(/ (+ (+ 5 4) (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (* (- 6 2) (- 2 7))))

;;ex-1.3
(define (func x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
	((and (< y x) (< y z)) (+ (* x x) (* z z)))
	((and (< z x) (< z y)) (+ (* x x) (* y y)))
	(else (+ (* x x) (* y y)))))
	 

;;ex-1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;ex-1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
;;無限ループする。引数yに渡した(p)を先に評価することになるため。

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt1 x)
  (sqrt-iter 1.0 x))

;;ex1-6
;;new-ifのelseが常に評価されるため、sqrt-iterが無限に呼び出される

;;ex1-7
;;小さい数の場合、例えば0.0000001の場合、推定値の二乗との差が0.001では差が大きすぎて精度が低い。
;;大きい数の場合、例えば200000000000000000000の場合、推定値の浮動小数の精度の限界で差が0.001になるような数値が存在しない場合がある。その場合、解が収束しなくなる。
;;前回との差分が一定値以下になった場合に収束とする方法では、上記のいずれの場合でも動作する。
(define (sqrt-iter2 prev guess x)
  (if (good-enough?2 prev guess)
      guess
      (sqrt-iter2 guess (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough?2 prev guess)
  (< (/ (abs (- prev guess)) (abs guess)) 0.001))

(define (sqrt2 x)
  (sqrt-iter2 0.0 1.0 x))

;;ex1-8
(define (cube x)
  (* x x x))

(define (cbrt x)
  (define (good-enough?3 prev guess)
    (< (/ (abs (- prev guess)) (abs guess)) 0.001))
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (define (cbrt-iter prev guess)
    (if (good-enough?3 prev guess)
	guess
	(cbrt-iter guess (improve guess))))
  (cbrt-iter 0.0 1.0))

;;ex1-9
;;再帰
(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;;反復
(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9

;;ex1-10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

;;2*n
(define (f n) (A 0 n))

;;2^n
(define (g n) (A 1 n))
(A 0 (A 1 (- n 1)))
(* 2 (A 0 (A 1 (- (- n 1) 1))))


;;0 (h 0)
;;2 (h 1)
;;2^(h (- n 1)
;;2 y=1
;;2^2 y=2
;;2^(2^2) y=3
;;2^(2^(2^2)) y=16 
(define (h n) (A 2 n))
(A 1 (h (- n 1)))

;;ex1-11

;;再帰
(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

;;反復
(define (f-loop n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

;;ex1-12
(define (pascal n m)
  (cond ((= m 1) 1)
	((= n m) 1)
	(else (+ (pascal (- n 1) (- m 1)) (pascal (- n 1) m)))))

;;ex1-13

;;Fib(n) = (φ^n - ψ^n)/√5
;;n = 0の場合
;;(1 - 1)/√5=0
;;n = 1の場合
;;((1+√5)/2-(1-√5)/2)/√5=1
;;n-1,nについて成立する場合、n+1で成立することを示す
;;(φ^n - ψ^n)/√5 + (φ^(n-1) - ψ^(n-1))/√5
;;(φ^(n-1)(1+φ) - ψ^(n-1)(1+ψ))/√5
;;(φ^(n+1) - ψ^(n+1))/√5
;;ψ^n/√5<0.5が成立すること、またFibは定義から整数であることより、Fibはφ^n/√5に最も近い整数である。

;;ex1-14
(define (count-charge amount) (cc amount 5))
(define (cc amount kind-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kind-of-coins 0)) 0)
	(else (+ (cc amount (- kind-of-coins 1)) (cc (- amount (first-denomination kind-of-coins)) kind-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

	
;;計算プロセスの木は省略

;;空間はΘ(n)。木の深さに比例するため。
;;ステップ数はΘ(n^5)。おおよそn/50*n/25*n/10*n/5*nの計算回数が必要なため。

;;ex1-15
;;a
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; pは5回適用される。
;;(sine 12.15)=(p (p (p (p (p (sine 0.05))))))

;;空間、ステップ数ともにΘ(log3_a)
;;
