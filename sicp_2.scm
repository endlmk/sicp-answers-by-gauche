;;ex2-1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n_r (/ n g))
	  (d_r (/ d g)))
      (if (< d_r 0)
	  (cons (* -1 n_r) (* -1 d_r))
	  (cons n_r d_r)))))

;;ex2-2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment s e)
  (cons s e))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (midpoint-segment seg)
  (let ((s (start-segment seg))
	(e (end-segment seg)))
    (make-point (/ (+ (x-point s) (x-point e)) 2.0) (/ (+ (y-point s) (y-point e)) 2.0))))

;;ex2-3
(define (make-rect s e)
  (cons s e))
(define (x-length r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (y-length r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))
(define (rect-perimeter r)
  (* 2 (+ (x-length r) (y-length r))))
(define (rect-area r)
  (* (x-length r) (y-length r)))
(define (make-rect2 c d)
  (cons c d))
(define (x-length r)
  (* (abs (x-point (cdr r))) 2))
(define (y-length r)
  (* (abs (y-point (cdr r))) 2))

;;ex2-4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

;;ex2-5
(define (cons1 a b)
  (* (expt 2 a) (expt 3 b)))
(define (car1 p)
  (define (car-for-expt p c)
    (cond ((= (mod p 2) 0) (car-for-expt (/ p 2) (+ c 1)))
	  (else c)))
  (car-for-expt p 0))
(define (cdr1 p)
  (define (cdr-for-expt p c)
    (cond ((= (mod p 3) 0) (cdr-for-expt (/ p 3) (+ c 1)))
	  (else c)))
  (cdr-for-expt p 0))

;;ex2-6
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))
(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;;ex2-7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

;;ex2-8
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
		 (- (upper-bound a) (lower-bound b))))

;;ex2-9
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(width (add-interval a b))
(/ (- (+ (upper-bound a) (upper-bound b)) (+ (lower-bound a) (lower-bound b))) 2)
(/ (+ (- (upper-bound a) (lower-bound a)) (- (upper-bound b) (lower-bound b))) 2)
(/ (+ (* (width a) 2) (* (width b) 2)) 2)
(width (sub-interval a b))
(/ (- (- (upper-bound a) (lower-bound b)) (- (lower-bound a) (upper-bound b))) 2)
(/ (+ (- (upper-bound b) (lower-bound b)) (- (upper-bound a) (lower-bound a))) 2)
(/ (+ (* (width a) 2) (* (width b) 2)))
;;mul-interval
;;(1 2) (3 4) -> (3 8) width:2.5
;;(1 2) (5 6) -> (5 12) width 3.5
;;div-interval
;;(1 2) (3 4) -> (1/4 2/3) width 5/24
;;(1 2) (5 6) -> (1/6 2/5) width 7/60

;;ex2-10
(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      0
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
		      (/ 1.0 (lower-bound y))))))

;;ex2-11
(define (mul-interval x y)
  (cond ((and (>= (lower-bound x) 0) (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	((and (< (upper-bound x) 0) (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
	((and (< (lower-bound x) 0) (< (lower-bound y) 0)) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (>= (lower-bound x) 0) (< (lower-bound y) 0)) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
	((and (< (* (lower-bound x) (upper-bound x)) 0) (>= (lower-bound y) 0)) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
	((and (< (upper-bound x) 0) (< (* (lower-bound y) (upper-bound y)) 0)) (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (< (* (lower-bound x) (upper-bound x)) 0) (< (lower-bound y) 0)) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (>= (lower-bound x) 0) (< (* (lower-bound y) (upper-bound y)) 0)) (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	(else (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))) (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))))

;;ex2-12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (make-center-percent c p)
  (let ((w (abs (* c (/ p 100)))))
    (make-center-width c w)))  
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
  (* (/ (width i) (abs (center i))) 100))

;;ex2-13
;;各インターバルの許容誤差をp1,p2とすると、積の許容誤差の近似値はp1+p2となる
;;(c1, p1) (c2, p2)として
;;mult = (c1 - c1*p1/100)*(c2 -c2*p2/100) (c1 + c1*p1/100)*(c2 + c2*p2/100)
;;≒ (c1 * c2 - (p1+p2)*c1*c2/100) (c1*c2 + (p1+p2)*c1*c2/100)
;;p = (((p1+p2)*c1*c2/100) / (c1*c2)) * 100 = p1+p2

;;ex2-14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))

;;ex2-15
;;区間の変数の数がpar1のほうが多い。区間の演算が多いほど誤差が広がるため、par1のほうが誤差が大きい

;;ex2-16
;;幅のある区間では、加法、乗法の逆源が存在しないため。一般の式に対して誤差が最小となるような代数的変形をコンピュータで実行することはできない。

;;ex2-17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

;;ex2-18
(define (reverse items)
  (define (reverse-iter a r)
    (if (null? a)
	r
	(reverse-iter (cdr a) (cons (car a) r))))
  (reverse-iter items ()))

;;ex2-19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount (except-first-domination coin-values))
	    (cc (- amount (first-domination coin-values)) coin-values)))))
(define (first-domination coins) (car coins))
(define (except-first-domination coins) (cdr coins))
(define (no-more? coins) (null? coins))
;;順序は影響しない。

;;ex2-20
(define (same-parity x . w)
  (define (filter-by-parity f w)
    (if (null? w)
	()
	(if (f (car w))
	    (cons (car w) (filter-by-parity f (cdr w)))
	    (filter-by-parity f (cdr w)))))
  (if (odd? x)
      (cons x (filter-by-parity odd? w))
      (cons x (filter-by-parity even? w))))

;;ex2-21
(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))

;;ex2-22
;;空リストに対してにconsで引数の先頭要素から結合するため、逆順になる
;;consの第一引数に結合されたリスト渡され、それらのが先頭がつながったリストになってしまう。

;;ex2-23
(define (for-each1 f items)
  (if (null? items)
      #t
      (and (f (car items)) (for-each1 f (cdr items)))))

;;ex2-24
;;(list 1 (list 2 (list 3 4)))
;; /\
;;1  (list 2 (list 3 4))
;;   /  \
;;  2    (list 3 4)
;;        /  \
;;        3   4

;;ex2-25
;;(1 3 (5 7) 9)
(car (cdr (car (cdr (cdr x)))))
;;((7))
(car (car x))
;;(1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

;;ex2-26
((append x y)) ;;(1 2 3 4 5 6)
(cons x y) ;; ((1 2 3) 4 5 6)
(list x y) ;; ((1 2 3) (4 5 6))

;;ex2-27
(define (deep-reverse items)
  (if (pair? items)
      (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
      items))
    
;;ex2-28
(define (fringe items)
  (if (pair? items)
      (append (fringe (car items)) (fringe (cdr items)))
      (if (null? items)
	  items
	  (list items))))

;;ex2-29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (cadr m))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cadr b))

(define (total-weight m)
  (let ((lb (branch-structure (left-branch m)))
	(rb (branch-structure (right-branch m))))
    (cond ((and (pair? lb) (pair? rb)) (+ (total-weight lb) (total-weight rb)))
	  ((pair? lb) (+ (total-weight lb) rb))
	  ((pair? rb) (+ lb (total-weight rb)))
	  (else (+ lb rb)))))

(define (balanced m)
  (let ((lb (branch-structure (left-branch m)))
	(ll (branch-length (left-branch m)))
	(rb (branch-structure (right-branch m)))
	(rl (branch-length (right-branch m)))))
  (cond ((and (pair? lb) (pair? rb)) (and (balanced lb) (balanced rb) (= (* ll (total-weight lb)) (* rl (total-weight rb)))))
	((pair? lb) (and (balanced lb) (= (* ll (total-weight lb)) (* rl rb))))
	((pair? rb) (and (balanced rb) (= (* ll lb) (* rl (total-weight rb)))))
	(else (= (* ll lb) (* rl rb)))))

;; listがconsになった場合、right-branch,branch-structureをcdrにすればよい

;;ex2-30
(define (square-tree1 tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree1 (car tree))
		    (square-tree1 (cdr tree))))))
(define (square-tree2 tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree2 sub-tree)
	     (square sub-tree)))
       tree))

;;ex2-31
(define (tree-map1 f tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (f tree))
	(else (cons (tree-map1 f (car tree))
		    (tree-map1 f (cdr tree))))))
(define (tree-map2 f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map2 f sub-tree)
	     (f sub-tree)))
       tree))

;;ex2-32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (item) (append item (list (car s)))) rest)))))
;; subsetsの対象のリストのうち、先頭とその後を分けて考える
;; subsetsの結果は先頭の後のリストに対してsubsetsした結果＋その結果に先頭の要素を加えたものとなるため。

;;ex2-33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length1 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;ex2-34
(define (horner-eval x coefficients-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
	      0
	      coefficients-sequence))

;;ex2-35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

;;ex2-36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;;ex2-37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(define (transpose mat)
  (accumulate-n cons () mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector m v)) cols)))

;;ex2-38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(fold-right / 1 (list 1 2 3)) ;; (1 / (2 / (3 / 1))) = 3/2
(fold-left / 1 (list 1 2 3)) ;; (((1 / 1) / 2) / 3) = 1/6
(fold-right list () (list 1 2 3)) ;; (1 (2 (3 ())))
(fold-left list () (list 1 2 3)) ;; (((() 1) 2) 3)
;; fold-leftとfold-rightが等しくなるためには(op x y) = (op y x)が必要

;;ex2-39
(define (reverse sequence)
  (fold-right (lambda (x y)  (append y (list x))) () sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))

;;ex2-40
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;ex2-41
(define (unique-triples n)
  (flatmap (lambda (i) (map (lambda (p) (cons i p)) (unique-pairs (- i 1)))) (enumerate-interval 1 n)))
(define (triple-sum triple)
  (+ (car triple) (cadr triple) (caddr triple)))
(define (find-triple-sum n s)
  (filter (lambda (t) (= (triple-sum t) s)) (unique-triples n)))

;;ex2-42
(define (queens board-size)
  (define empty-board (map (lambda (p) 0) (enumerate-interval 1 board-size)))
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position
		    new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position n k r)
  (define (adjoin-position-iter p n k r)
    (cond ((null? r) ())
	  ((= p k) (cons n (adjoin-position-iter (+ p 1) n k (cdr r))))
	  (else (cons (car r) (adjoin-position-iter (+ p 1) n k (cdr r))))))
  (adjoin-position-iter 1 n k r))

(define (safe? k position)
  (let ((kpos (list-ref position (- k 1))))
    (define (safe?-iter p position)
      (let ((target (car position)))
	(if (= p k)
	    #t
	    (if (or (= target kpos) (= target (- kpos (- k p))) (= target (+ kpos (- k p))))
		#f
		(safe?-iter (+ p 1) (cdr position))))))
    (safe?-iter 1 position)))
  
;;ex2-43
;;queen-colでkを一つ減らす際に毎回k=0までのqueen-colの結果を計算している
;;それぞれのqueen-colでさらにqueen-colを呼び出すため、T * board-size ^ board-sizeとなる。
