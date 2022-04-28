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
  (require (> (+ h 1) l))
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

;;ex4.40
;;前5^5
;;後5!
(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (not (= cooper fletcher)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
	(require (not (= smith fletcher)))
	(require (not (= smith cooper)))
	(require (not (= (abs (- smith fletcher)) 1)))
	(let ((miller (amb 1 2 3 4 5)))
	  (require (not (= miller fletcher)))
	  (require (not (= miller cooper)))
	  (require (not (= miller smith)))		   
	  (require (> miller cooper))
	  (let ((baker (amb 1 2 3 4 5)))
	    (require (not (= baker 5)))
	    (require (not (= baker fletcher)))
	    (require (not (= baker cooper)))
	    (require (not (= baker smith)))
	    (require (not (= baker miller)))
	    (list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith))))))))

;;ex4.41
(define (EnumList vals lists)
  (Flatten (map (lambda (x) (map (lambda (l) (cons x l)) lists)) vals)))
(define (Flatten listoflist)
  (if (null? listoflist)
      ()
      (append (car listoflist) (Flatten (cdr listoflist)))))
(define (EnumAllCandidate)
  (let ((c (list 1 2 3 4 5)))
    (EnumList c
	      (EnumList c
			(EnumList c
				  (EnumList c '((1) (2) (3) (4) (5))))))))
(define (Solve-Dwelling)
  (define (baker l) (car l))
  (define (cooper l) (cadr l))
  (define (fletcher l) (caddr l))
  (define (miller l) (cadddr l))
  (define (smith l) (cadr (cdddr l)))
  (define (cond1 l) (not (= (baker l) 5)))
  (define (cond2 l) (not (= (cooper l) 1)))
  (define (cond3 l) (not (= (fletcher l) 5)))
  (define (cond4 l) (not (= (fletcher l) 1)))
  (define (cond5 l) (> (miller l) (cooper l)))
  (define (cond6 l) (not (= (abs (- (smith l) (fletcher l))) 1)))
  (define (cond7 l) (not (= (abs (- (fletcher l) (cooper l))) 1)))
  (define (cond8 l) (distinct? l))
  (define (distinct? items)
    (cond ((null? items) #t)
	  ((null? (cdr items)) #t)
	  ((member (car items) (cdr items)) #f)
	  (else (distinct? (cdr items)))))
  (let ((ans (car (filter (lambda (l) (and (cond8 l)
					   (cond1 l)
					   (cond2 l)
					   (cond3 l)
					   (cond4 l)
					   (cond5 l)
					   (cond6 l)
					   (cond7 l)))
			  (EnumAllCandidate)))))
    (list (list 'baker (baker ans))
	  (list 'cooper (cooper ans))
	  (list 'fletcher (fletcher ans))
	  (list 'miller (miller ans))
	  (list 'smith (smith ans)))))

;;ex4.42
(define (solve-phillips)
  (define (require-one l1 l2) (require (if l1 (not l2) l2)))
  (let ((Betty (amb 1 2 3 4 5))
	(Ethel (amb 1 2 3 4 5))
	(Joan (amb 1 2 3 4 5))
	(Kitty (amb 1 2 3 4 5))
	(Mary (amb 1 2 3 4 5)))
    (require (distinct? (list Betty Ethel Joan Kitty Mary)))
    (require-one (= Kitty 2) (= Betty 3))
    (require-one (= Ethel 1) (= Joan 2))
    (require-one (= Joan 3) (= Ethel 5))
    (require-one (= Kitty 2) (= Mary 4))
    (require-one (= Mary 4) (= Betty 1))
    (list (list 'Betty Betty)
	  (list 'Ethel Ethel)
	  (list 'Joan Joan)
	  (list 'Kitty Kitty)
	  (list 'Mary Mary))))
	       
;;ex4.43
(define (solve-cruiser)
  (let ((Moore (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa))
	(MooreC (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa)))
    (require (eq? Moore 'Mary))
    (require (eq? MooreC 'Lorna))
    (require (not (eq? Moore MooreC)))
    (let ((Barnacle (amb 'Mary 'Gabrille 'Lorna 'Rosalind 'Mellisa))
	  (BarnacleC (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa)))
      (require (eq? BarnacleC 'Gabrielle))
      (require (eq? Barnacle 'Mellisa))
      (require (not (eq? Barnacle Moore)))
      (require (not (eq? BarnacleC MooreC)))
      (let ((Hall (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa))
	    (HallC (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa)))
	(require (eq? HallC 'Rosalind))
	(require (not (eq? Hall HallC)))
	(require (not (eq? Hall Moore)))
	(require (not (eq? Hall Barnacle)))
	(require (not (eq? HallC MooreC)))
	(require (not (eq? HallC BarnacleC)))
	(let ((Downing (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa))
	      (DowningC (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa)))
	  (require (eq? DowningC 'Mellisa))
	  (require (not (eq? Downing DowningC)))
	  (require (not (eq? Downing Moore)))
	  (require (not (eq? Downing Barnacle)))
	  (require (not (eq? Downing Hall)))
	  (require (not (eq? DowningC MooreC)))
	  (require (not (eq? DowningC BarnacleC)))
	  (require (not (eq? DowningC HallC)))
	  (let ((Parker (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa))
		(ParkerC (amb 'Mary 'Gabrielle 'Lorna 'Rosalind 'Mellisa)))
	    (require (not (eq? Parker ParkerC)))
	    (require (not (eq? Parker Moore)))
	    (require (not (eq? Parker Barnacle)))
	    (require (not (eq? Parker Hall)))
	    (require (not (eq? Parker Downing)))
	    (require (not (eq? ParkerC MooreC)))
	    (require (not (eq? ParkerC BarnacleC)))
	    (require (not (eq? ParkerC HallC)))
   	    (require (not (eq? ParkerC DowningC)))
	    (require (eq? Parker (cond ((eq? 'Gabrielle Moore) MooreC)
				     ((eq? 'Gabrielle Barnacle) BarnacleC)
				     ((eq? 'Gabrielle Hall) HallC)
				     (else DowningC))))
	    (cond ((eq? 'Lorna Moore) 'Moore)
		  ((eq? 'Lorna Barnacle) 'Barnacle)
		  ((eq? 'Lorna Hall) 'Hall)
		  ((eq? 'Lorna Downing) 'Downing)
		  (else 'Parker))))))))
;; Downing
;; MaryがMooreの娘とわからない場合、DowningとParkerが解となる。

;;ex4.44
(define (list-ref list p)
  (if (= p 0) (car list)
      (list-ref (cdr list) (- p 1))))
(define (safe? k position)
  (let ((kpos (list-ref position (- k 1))))
    (define (safe?-iter p position)
      (let ((target (car position)))
	(if (= p k)
	    true
	    (if (or (= target kpos) (= target (- kpos (- k p))) (= target (+ kpos (- k p))))
		false
		(safe?-iter (+ p 1) (cdr position))))))
    (safe?-iter 1 position)))
(define (solve-8queen)
  (let ((pos (list (an-integer-between 1 8)
		   (an-integer-between 1 8)
		   (an-integer-between 1 8)
		   (an-integer-between 1 8)
		   (an-integer-between 1 8)
		   (an-integer-between 1 8)
		   (an-integer-between 1 8)
		   (an-integer-between 1 8))))
    (define (iter k position)
      (if (safe? k position)
	  (if (= k 8)
	      true
	      (iter (+ k 1) position))
	  false))
    (require (iter 1 pos))
    pos))
  
;;natural language parser
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define (parse-sentence)
  (list 'sentence
	(parse-noun-phrase)
	(parse-verb-phrase)))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
	(parse-word prepositions)
	(parse-noun-phrase)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
	(parse-word articles)
	(parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
	 (maybe-extend
	  (list 'noun-phrase
		noun-phrase
		(parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
	 (maybe-extend
	  (list 'verb-phrase
		verb-phrase
		(parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-word word-list)
  (require (not (null? *unphrased*)))
  (require (memq (car *unphrased*) (cdr word-list)))
  (let ((found-word (car *unphrased*)))
    (set! *unphrased* (cdr *unphrased*))
    (list (car word-list) found-word)))
(define *unphrased* '())
(define (parse input)
  (set! *unphrased* input)
  (let ((sent (parse-sentence)))
    (require (null? *unphrased*))
    sent))

;; 教授が猫と一緒に生徒に講義を行う。
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (simple-noun-phrase (article the) (noun student))))
  (prep-phrase
   (prep with)
   (simple-noun-phrase (article the) (noun cat)))))
;; 教授が「猫と一緒の学生」に講義を行う。
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase
   (prep to)
   (noun-phrase
    (simple-noun-phrase (article the) (noun student))
    (prep-phrase
     (prep with)
     (simple-noun-phrase (article the) (noun cat)))))))

;;ex4.45
;;教授が猫と一緒に、教室で学生に講義を行う
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (prep-phrase
     (prep to)
     (simple-noun-phrase (article the) (noun student))))
   (prep-phrase
    (prep in)
    (simple-noun-phrase (article the) (noun class))))
  (prep-phrase
   (prep with)
   (simple-noun-phrase (article the) (noun cat)))))
;;教授が猫のいる教室で学生に講義を行う
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (simple-noun-phrase (article the) (noun student))))
  (prep-phrase
   (prep in)
   (noun-phrase
    (simple-noun-phrase (article the) (noun class))
    (prep-phrase
     (prep with)
     (simple-noun-phrase (article the) (noun cat)))))))
;;教授が猫と一緒に、教室にいる学生に講義を行う
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (simple-noun-phrase (article the) (noun student))
     (prep-phrase
      (prep in)
      (simple-noun-phrase (article the) (noun class))))))
  (prep-phrase
   (prep with)
   (simple-noun-phrase (article the) (noun cat)))))
;;教授が、猫と一緒に教室にいる学生に講義を行う
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase
   (prep to)
   (noun-phrase
    (noun-phrase
     (simple-noun-phrase (article the) (noun student))
     (prep-phrase
      (prep in)
      (simple-noun-phrase (article the) (noun class))))
    (prep-phrase
     (prep with)
     (simple-noun-phrase (article the) (noun cat)))))))
;;教授が猫がいる教室の学生に講義を行う。
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase
   (prep to)
   (noun-phrase
    (simple-noun-phrase (article the) (noun student))
    (prep-phrase
     (prep in)
     (noun-phrase
      (simple-noun-phrase (article the) (noun class))
      (prep-phrase
       (prep with)
       (simple-noun-phrase (article the) (noun cat)))))))))

;;ex4.46
;;被演算子が右から左に評価されるとする
;;この場合、(parse-sentence)は(parse-verb-phrase)(parse-noun-phrase)の順に評価される
;;the cat eatsをparseすると
;;verbのparseで失敗ｰ>nounのparseで成功ｰ>*unparsed*にeatsが残り、失敗となる
;;parseは入力を左から順に取り出し、左から解釈することを意図して書かれているため、評価順は左から右でなければならない。

;;ex4.47
;;解析対象の入力の最初が動詞でなかった場合、無限ループする。
;;lecture to the studentの場合、lectureは成功し、次の解析を行う際、
;;to the studentをparse-verb-pharseで解析することになり、無限ループに陥る
;;引数を入れ替えても無限ループする

;;ex4.48
(define adjectives '(adjective small big))
(define adverbs '(adverb quickly hardly))

(define (parse-adjective-noun-phrase)
  (list 'adjective-noun-phrase
	(parse-word articles)
	(parse-word adjectives)
	(parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
	 (maybe-extend
	  (list 'noun-phrase
		noun-phrase
		(parse-prepositional-phrase)))))
  (let ((noun-phrase (amb (parse-simple-noun-phrase)
			  (parse-adjective-noun-phrase))))
    (maybe-extend noun-phrase)))
(define (parse-adverb-verb-phrase)
  (list 'adverb-verb-phrase
	(parse-word verbs)
	(parse-word adverbs)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
	 (maybe-extend
	  (list 'verb-phrase
		verb-phrase
		(parse-prepositional-phrase)))))
  (let ((verb-phrase (amb (parse-word verbs)
			  (parse-adverb-verb-phrase))))
    (maybe-extend verb-phrase)))

;;ex4.49
(define (flatten-amb l)
  (if (null? l)
      (amb)
      (amb (car l) (flatten-amb (cdr l)))))
(define (parse-word word-list)
  (require (not (null? *unphrased*)))
  (set! *unphrased* (cdr *unphrased*))
  (list (car word-list) (flatten-amb (cdr word-list))))

;;ex4.50
(use srfi-27)
(define (ramb-choices exp)
  (define (random-mix l)
    (define (random-choice l)
      (let ((choice (random-integer (length l))))
	(define (iter l prev c)
	  (if (= c 0)
	      (cons (car l) (append prev (cdr l)))
	      (iter (cdr l) (append prev (list (car l))) (- c 1))))
	(iter l () choice)))
    (if (null? l)
	()
	(let ((p (random-choice l)))
	  (cons (car p) (random-mix (cdr p))))))
  (let ((choices (cdr exp)))
    (random-mix choices)))

;; analyze
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
      	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
      	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((let? exp) (analyze (let->combination exp)))
	((and? exp) (analyze (and->if exp)))
	((or? exp) (analyze (or->if exp)))
	((amb? exp) (analyze-amb exp))
	((ramb? exp) (analyze-ramb exp))
      	((application? exp) (analyze-application exp))
	(else (error "Unknown expression type: ANALYZE" exp))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices)
	    (fail)
	    ((car choices) env succeed (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (flatten-ramb l)
  (if (null? l)
      (ramb)
      (ramb (car l) (flatten-ramb (cdr l)))))
(define (parse-word word-list)
  (require (not (null? *unphrased*)))
  (let ((word (flatten-ramb (cdr word-list))))
    (set! *unphrased* (cdr *unphrased*))
    (list (car word-list) word)))
