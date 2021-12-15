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

;;ex2-44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (besides smaller smaller)))))

;;ex2-45
(define (split painter io so n)
  (if (= n 0)
      painter
      (let ((smaller (split io so (- n 1))))
	(io painter (so smaller smaller)))))

;;ex2-46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v1)
  (make-vect (* s (xcor-vect v1)) (* s (ycor-vect v1))))

;;ex2-47
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
	       (scale-vect (ycor-vect v) (edge2-frame frame))))))
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

(define (maek-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (cddr f))

;;ex2-48
(define (make-segment s e)
  (cons s e))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;;ex2-49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame)
	 (start-segment segment))
	((frame-coord-map frame)
	 (end-segment segment))))
     (segment-list))))
;;a
(define edge-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 0))
	 (make-segment (make-vect 0 0) (make-vect 0 1))
	 (make-segment (make-vect 1 0) (make-vect 1 1))
	 (make-segment (make-vect 0 1) (make-vect 1 1)))))
;;b
(define diag-frame-painter
  (segments->painter
   (list (make-segment (make-vect 1 0) (make-vect 0 1))
	 (make-segment (make-vect 0 0) (make-vect 1 1)))))
;;c
(define mid-rhombus-painter
  (segments->painter
   (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
	 (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
	 (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
	 (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))
;;d
(define wave
  (segments->painter
   (list (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
         (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
         (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65))
         (make-segment (make-vect 0.40 0.65) (make-vect 0.30 0.65))
         (make-segment (make-vect 0.75 0.65) (make-vect 1.00 0.35))
         (make-segment (make-vect 0.60 0.45) (make-vect 1.00 0.15))
         (make-segment (make-vect 0.60 0.45) (make-vect 0.75 0.00))
         (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
         (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.60))
         (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40))
         (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85))
         (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65))
         (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.50))
         (make-segment (make-vect 0.35 0.50) (make-vect 0.25 0.00))
         (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00)))))

;;ex2-50
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter (make-frame
		  new-origin
		  (sub-vect (m corner1) new-origin)
		  (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (shirink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
(define (rotate-180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))
(define (rotate-270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
	   (transform-painter painter1
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0)))
	  (paint-botom
	   (transform-painter painter2
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point)))
      (lambda (frame)
	(paint-top frame)
	(paint-bottom frame)))))
(define (below painter1 painter2)
  (rotate-270 (beside (rotate-90 painter1) (rotate-90 painter2))))

;;ex2-52
(define wave1
  (segments->painter
   (list (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
         (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
         (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65))
         (make-segment (make-vect 0.40 0.65) (make-vect 0.30 0.65))
         (make-segment (make-vect 0.75 0.65) (make-vect 1.00 0.35))
         (make-segment (make-vect 0.60 0.45) (make-vect 1.00 0.15))
         (make-segment (make-vect 0.60 0.45) (make-vect 0.75 0.00))
         (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
         (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.60))
         (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40))
         (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85))
         (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65))
         (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.50))
         (make-segment (make-vect 0.35 0.50) (make-vect 0.25 0.00))
         (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00))
	 (make-segment (make-vect 0.44 0.70) (make-vect 0.51 0.70))))

(define (corner-split1 painter n)
  (if (= n 0)
      painter
      (beside (below painter (up-split painter (- n 1)))
	      (below (right-split painter (- n 1)) (corner-split painter (- n 1))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate-180
				  identity flip-holiz)))
    (combine4 (corner-split painter n))))
    
;;ex2-53
(list 'a 'b 'c) ;;(a b c)
(list (list 'george)) ;;((george))
(cdr '((x1 x2) (y1 y2))) ;;((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;;(y1 y2)
(pair? (car '(a short list))) ;;#f
(memq 'red '((red shoes) (blue shoes))) ;;#f
(memq 'red '(red shoes blue socks)) ;;(red shoes blue socks

;;ex2-54
(define (equal? a b)
  (cond ((eq? a b) #t)
	(else (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))))

;;ex2-55
(car ''abracadabra) ;; (car '(quote abracadabra)) -> quote

;;ex2-56
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((product? exp) (make-sum (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
				  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
	((exponentiation? exp) (make-product
				(make-product (exponent exp)
					      (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
				(deriv (base exp) var)))
	(else (error "unknown expresssion type: DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	(else (list '** base exponent))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

;;ex2-57
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((product? exp) (make-sum (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
				  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
	(else (error "unknown expresssion type: DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;;ex2-58
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	((product? exp) (make-sum (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
				  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
	(else (error "unknown expresssion type: DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;;ex2-59
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
	((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
  (cond ((null? set2) set1) 
	((null? set1) set2)
	((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

;;ex2-60
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cons x set))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
	((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
  (append set1 set2))

;;ex2-61
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      ()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2) (intersection-set (cdr set1) set2))
	      ((> x1 x2) (intersection-set set1 (cdr set2)))))))
(define (adjoin-set x set)
  (cond ((null? set) (cons x ()))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))
;;ex2-62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
		      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
		      ((> x1 x2) (cons x2 (cons x1 (union-set (cdr set1) (cdr set2))))))))))

;;ex2-63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set)) (element-of-set? x (left-branch set)))
	((> x (entry set)) (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x () ()))
	((= x (entry set)) set)
	((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
	((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      ()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree) (tree->list-1 (right-branch tree))))))
;;(1 3 5 7 9 11)
;;(1 3 5 7 9 11)
;;(1 3 5 7 9 11)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree ()))
;;(1 3 5 7 9 11)
;;(1 3 5 7 9 11)
;;(1 3 5 7 9 11)

;;ツリーの走査のオーダーは同じ。appendの分、1のほうが遅い。

;;ex2-64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons () elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts) right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

;;(1 3) (5...)
;;(1 () ()) (3) (5..)
;;(3 (1 () ()) ()) (5..)
;;(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))
;;リストの中央の要素から左と右に分け、それぞれで部分列を作る処理を再帰的に実行する。
;;要素数分のツリーを作るので要素数のオーダーとなる。

;;ex2-65
(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (define (union-list s1 s2)
      (cond ((null? s1) s2)
	    ((null? s2) s1)
	    (else (let ((x1 (car s1))
			(x2 (car s2)))
		    (cond ((= x1 x2) (cons x1 (union-list (cdr s1) (cdr s2))))
			  ((< x1 x2) (cons x1 (union-list (cdr s1) s2)))
			  ((> x1 x2) (cons x2 (cons x1 (union-list (cdr s1) (cdr s2))))))))))
    (list->tree (union-list list1 list2))))
(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (define (intersection-list set1 set2)
      (if (or (null? set1) (null? set2))
	  ()
	  (let ((x1 (car set1))
		(x2 (car set2)))
	    (cond ((= x1 x2) (cons x1 (intersection-list (cdr set1) (cdr set2))))
		  ((< x1 x2) (intersection-list (cdr set1) set2))
		  ((> x1 x2) (intersection-list set1 (cdr set2)))))))
    (list->tree (intersection-list list1 list2))))

;;ex2-66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((= given-key (key (entry set-of-records))) (entry set-of-records))
	((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records)))))

;;ex2-67
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit: CHOOSE-BRANCH" bit))))
(define sample-tree (make-code-tree (make-leaf 'A 4)
				     (make-code-tree (make-leaf 'B 2)
						     (make-code-tree (make-leaf 'D 1)
								     (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)) 
;;ADABBCA

;;ex2-68
(define (encode message tree)
  (if (null? message)
      ()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (encode-symbol1 symbol current result)
    (if (leaf? current)
	(if (eq? symbol (symbol-leaf current))
	    result
	    #f)
	(let ((l_result (encode-symbol1 symbol (left-branch current) (append result '(0))))
	      (r_result (encode-symbol1 symbol (right-branch current) (append result '(1)))))
	  (cond ((pair? l_result) l_result)
		((pair? r_result) r_result)
		(else #f)))))
  (encode-symbol1 symbol tree ()))

;;ex2-69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      ()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leafset)
  (if (null? (cdr leafset))
      (car leafset)
      (successive-merge (adjoin-set (make-code-tree (car leafset) (cadr leafset)) (cddr leafset)))))

;;ex2-70
(define rock-list '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define rock-tree (generate-huffman-tree rock-list))

;;GET A JOB -> (1 1 1 1 1 1 1 0 0 1 1 1 1 0) 14, fixed bits 9
;;SHA NA NA NA NA NA NA NA NA -> (1 1 1 0 0 0 0 0 0 0 0 0) 12, fixed bits 27 
;;WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP -> (1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0) 23, fixed bits 30
;;SHA BOOM -> (1 1 1 0 1 1 0 1 0) 9, fixed bits 6

;;ex2-71
;;((A 16) (B 8) (C 4) (D 2) (E 1))
;;((A 16) (B 8) (C 4) ({D E} 3))
;;((A 16) (B 8) ({C D E} 7))
;;((A 16) ({B C D E} 15))
;;(({A B C D E} 31))
;;頻度最高:1bit 頻度最低:4bit

;;((A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) (H 4) (I 2) (J 1))
;;((A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) (H 4) ({I J} 3))
;;((A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) ({H I J} 7))
;;((A 512) (B 256) (C 128) (D 64) (E 32) (F 16) ({G H I J} 15))
;;((A 512) (B 256) (C 128) (D 64) (E 32) ({F G H I J} 31))
;;((A 512) (B 256) (C 128) (D 64) ({E F G H I J} 63))
;;((A 512) (B 256) (C 128) ({D E F G H I J} 127))
;;((A 512) (B 256) ({C D E F G H I J} 255))
;;((A 512) ({B C D E F G H I J} 511))
;;(({A B C D E F G H I J} 1023))
;;頻度最高:1bit 頻度最低:9bit

;;ex2-72
;;最高頻度->1のオーダー
;;最低頻度->n*(n-1)/2のオーダー

;;ex2-73
;;a
;;演算子(operator)を選択して、対応する微分演算を演算対象(operands)に適用している
;;number?, variable?が処理するexpは(operator operands)のペアではない。そのため同じ形式でデータ主導ディスパッチに取り込めない
;;b
(define (install-sum-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
	      (deriv (cadr operands) var)))
  (put 'deriv '+ deriv-sum))
(define (install-product-package)
  (define (deriv-product operands var)
    (make-sum (make-product (car operands) (deriv (cadr operands) var))
	      (make-product (deriv (car operands) var) (cadr operands))))
  (put 'deriv '* deriv-product))
;;c
(define (install-exponentiation-package)
  (define (deriv-exponentiation operands var)
    (make-product (make-product (cadr operands)
				(make-exponentiation (car operands) (make-sum (cadr operands) -1)))
		  (deriv (car operands) var)))
  (put 'deriv '** deriv-exponentiation))
;;d
;;putの第一引数と第二引数を入れ替える必要がある。

