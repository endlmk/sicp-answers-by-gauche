;;ex4.25
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
;;適用順序の場合、unlessのusual,exceptionalどちらも評価を終える必要があるが、usualでfactorialを再帰させているため、無限に再帰する。
;;正規順序の場合、n=1の場合にunlessの評価が完了し、正しく動作する。

;;ex4.26
(define (unless->if exp)
  (let ((pproc (cadr exp))
	(cproc (cadddr exp))
	(aproc (caddr exp)))
    (make-if pproc cproc aproc)))
;;手続きだとmapのような高階関数に渡すことができる
(map unless '(#t #t #f #f) '(1 2 3 4) '(5 6 7 8)) 

;;ex4.27
;;; L-Eval input:
(define count 0)
;;; L-Eval input:
(define (id x) (set! count (+ count 1)) x)
;;; L-Eval input:
(define w (id (id 10))) ;;<-この時点では外側のidを評価した際にset!が呼ばれる。内側のidはサンクとして環境に配置され、未評価の状態である。
;;; L-Eval input:
count
;;; L-Eval value:
1 ;;<-上述の通り、set!が一度だけ呼ばれるためcountは1となる
;;; L-Eval input:
w ;;<-この際にdriver-loopのactual-valueでサンクである内側のidがforce-itされることでset!が呼ばれる
;;; L-Eval value:
10
;;; L-Eval input:
count
;;; L-Eval value:
1 ;;<-上述の通り、wを表示した際にset!が呼ばれるためcountは2となる
