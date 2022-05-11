;;database
(assert! (address (Bitdiddle Ben) (slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor Fect Cy D) (Bitdiddle Ben))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

;;ex4.55
;;1
(supervisor ?x (Bitdiddle Ben))
;;2
(job ?x (accounting . ?type))
;;3
(address ?x (Slumerville . ?type))

;;ex4.56
;;1
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where)
;;2
(and (salary (Bitdiddle Ben) ?amount-ben)
     (salary ?person ?amount)
     (lisp-value < ?amount ?amount-ben))
;;3
(and (not (job ?boss (conputer . ?type)))
     (supervisor ?person ?boss))

;;ex4.57
(rule (replace ?person ?person2)
      (and (job ?person ?job)
           (or (job ?person2 ?job)
               (and (can-do-job ?job ?job2)
                    (job ?person2 ?job2)))
           (not (same ?person ?person2))))
;;1
(replace (Fect Cy D) ?p)
;;2
(and (replace ?p1 ?p2)
     (salary ?p1 ?saraly1)
     (salary ?p2 ?salary2)
     (lisp-value ?salary1 < ?salary2))

;;ex4.58
(rule (big-shot ?person)
     (and (supervisor ?person . ?boss)
          (lisp-value null? ?boss)))

;;ex4.59
;;a
(and (meeting ?job ?datetime)
     (job ?person (?job . ?type))
     (same ?person (Ben Bitdiddle)))
;;b
(rule (meeting-time ?person ?day-and-times)
      (and (meeting ?job ?day-and-time)
           (job ?person (?job . ?type)))
;;c
(meeting-time (Hacker Alyssa P) (Wednesday . ?time))

;;ex4.60
;;DBの各レコードに対してクエリが一致するか適用する。
;;そのため、順序が入れ替わった場合でもクエリの条件を満たすことになるため
;;person1とperson2を順序づけるようにすればよい
(and (lives-near ?person1 ?person2)
     (lisp-value compare ?person1 ?person2))

;;ex4.61
(?x next-to ?y in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))
(?x next-to 1 in (2 1 3 1))
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))

;;ex4.62
(rule (last-pair (?x. ()) (?x))
(rule (last-pair (?x . ?v))
      (last-pair ?v))
;;(last-pair ?x (3))に対しては(last-pair (3) (3))しか返らない。
;;最後が3で他が任意の要素であるリストが条件を満たすが、クエリからは(3)しか得られない。

;;ex4.63
(rule (grand-child ?p ?c)
     (and (son-of-parent ?p ?p1)
          (son-of-parent ?p1 ?c)))
(rule (son-of-parent ?p ?c)
     (or (son ?p ?c)
         (and (wife ?p ?w)
              (son ?w ?c))))

;;ex4.64
;;outranked-byがorで結合され、一方のクエリの最初に再帰的に評価するように書かれているので、無限ループする

;;ex4.65
;;Warbucksがsupervisorであるという表明が4つあり、それらに対応する結果をすべて出力してしまうため。

;;ex.4.66
;;4.65のように重複のある結果を返すケースでは、正しい集積結果が得られない
;;重複のある結果を重複のない一意の要素だけがある結果にする処理があればよい。

;;ex4.67


;;ex4.68
(rule (reverse () ()))
(rule (reverse (?u . ?v) ?reversed)
     (and (reverse ?v ?prereversed)
          (append-to-form ?prereversed (?u) ?reversed)))
;;(reverse ?x (1 2 3))では第一引数が未束縛のままreverseを再帰的に呼び出すため無限ループする

;;ex4.70
;;以下のように実装すると、THE-ASSERTIONSは、assertionを無限に返す無限ストリームとなってしまう。
(set! THE-ASSERTONS
      (cons-stream assertion THE-ASSERTIONS))
;;一度局所変数に入れることでストリームへのconsを実現している

