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

;;ex1-16
(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter a (* b b) (/ n 2)))
	(else (fast-expt-iter (* a b) b (- n 1)))))
(define (fast-expt b n) (fast-expt-iter 1 b n))

;;ex1-17
(define (** a b)
  (if (= b 0)
      0
      (+ a (** a (- b 1)))))
(define (fast** a b)
  (cond ((= b 0) 0)
	((even? b) (+ (fast** a (/ b 2)) (fast** a (/ b 2))))
	(else (+ a (fast** a (- b 1))))))

;;ex1-18
(define (fast**-iter r a b)
  (cond ((= b 0) r)
	((even? b) (fast**-iter r (+ a a) (/ b 2)))
	(else (fast**-iter (+ r a) a (- b 1)))))    
(define (fast**2 a b) (fast**-iter 0 a b))

;;ex1-19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))
		   (+ (* q q) (* 2 p q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;;ex1-20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;;正規順序評価
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0) (remainder 206 40) (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40))) ;;1
(if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;;1
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) ;;3
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;;7
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)) (remainder  (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))) ;;7
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)));;14
2 ;;18

;;適用順序評価
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))
(gcd 40 6);;1
(if (= 6 0) 40 (gcd 6 (remainder 40 6)));;1
(gcd 6 (remainder 40 6));;1
(gcd 6 4);;2
(if (= 4 0) 6 (gcd 4 (remainder 6 4)));;2
(gcd 4 (remainder 6 4));;2
(gcd 4 2);;3
(if (= 2 0) 4 (gcd 2 (remainder 4 2)));;3
(gcd 2 (remainder 4 2));;3
(gcd 2 0);;4
(if (= 0 0) 2 (gcd 0 (remainder 2 0)));;4
2;;4

;;ex1-21
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

;;ex1-22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (search-for-primes a b)
  (cond ((even? a) (search-for-primes (+ a 1) b))
	((> a b) #f)
	(else (begin (timed-prime-test a)
		     (search-for-primes (+ a 2) b)))))

;;ex1-23
(define (smallest-divisor1 n) (find-divisor1 n 2))
(define (find-divisor1 n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor1 n (if (= test-divisor 2) 3 (+ test-divisor 2))))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor1 n)))

;;ex1-24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (random num)
  (use srfi-27)
  (* (random-integer num) 1.0))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))
(define (prime? n)
  (fast-prime? n 1))

;;ex1-25
;;fast-expを使うと、累乗を計算したあとにmodを取るため、累乗の計算でオーバーフローが起こりうる。
;;expmodは逐次modを取るため、値が大きくなりすぎてオーバーフローすることがない。

;;ex1-26
;;二乗の際にsquareを使わず、expmodを二回呼び出している
;;logn回の再帰プロセスがそれぞれ二回expmodを呼び出すため、2^logn回の呼び出しとなり、結果Θ(n)となる。

;;ex1-27
(define (fermat-test a n)
  (= (expmod a n n) a))
(define (test-carmichael n)
  (fermat-test-iter 1 n))
(define (fermat-test-iter a n)
  (cond ((= a n) #t)
	(else (if (fermat-test a n)
		  (fermat-test-iter (+ a 1) n)
		  #f))))

;;ex1-28
(define (expmod-s base exp m)
  (cond ((= exp 0) 1)
	(else (if (even? exp)
		  (if (and (not (= base 1)) (not (= base (- m 1))) (= (remainder (square base) m) 1))
		      0
		      (remainder (square (expmod-s base (/ exp 2) m)) m))
		  (remainder (* base (expmod-s base (- exp 1) m)) m)))))
(define (fermat-test-m n)
  (define (try-it a)
    (= (expmod-s a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime?-m n times)
  (cond ((= times 0) #t)
	((fermat-test-m n) (fast-prime?-m n (- times 1)))
	(else #f)))
(define (prime?-m n)
  (fast-prime?-m n 10))

;;ex1-29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (inc n) (+ n 1))
(define (simpson f a b n)
  (define (simpson-iter count)
    (cond ((= count 0) (f a))
	  ((= count n) (f b))
	  ((even? count) (* 2 (f (+ a (* count (/ (- b a) n)))))) 
	  (else (* 4 (f (+ a (* count (/ (- b a) n))))))))
  (* (/ (/ (- b a) n) 3.0) (sum simpson-iter 0 inc n)))

;;ex1-30
(define (sum-loop term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))
	
;;ex1-31
;;a
(define (id x) x)
(define (product f a next b)
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))
(define (factorial n) (product id 1 inc n))
(define (termforpi n)
  (/ (* (+ (div n 2) 1) 2.0) (+ (* (div (+ n 1) 2) 2) 1.0)))
(define (calcpi n) (product termforpi 1 inc n))

;;b
(define (product-loop f a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (f a)))))
  (iter a 1))

;;ex1-32
;;a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (add a b) (+ a b))
(define (sum1 term a next b) (accumulate add 0 term a next b))
(define (mult a b) (* a b))
(define (product1 term a next b) (accumulate mult 1 term a next b))
;;b
(define (accumulate-loop combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;ex1-33
(define (filtered-accumulate combiner null-value term a next b filt)
  (if (> a b)
      null-value
      (if (filt a)
	  (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filt))
	  (filtered-accumulate combiner null-value term (next a) next b filt))))
(define (sum-primes a b) (filtered-accumulate add 0 id a inc b prime?))
(define (product-coprime n)
  (define (coprime? a)
    (= (gcd a n) 1))
  (filtered-accumulate mult 1 id 1 inc n coprime?))

;;ex1-34
(define (f g) (g 2))
(f f)
(f (lambda (g) (g 2)))
((lambda (g) (g 2)) 2)
(2 2)
