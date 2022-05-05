;;ex4.55
;;1
(supervisor ?x (Ben Bitdiddle))
;;2
(job ?x (accounting . ?type))
;;3
(address ?x (Slumerville . ?type))

;;ex4.56
;;1
(and (supervisor ?person (Ben Bitdiddle))
     (address ?person ?where)
;;2
(and (salary (Ben Bitdiddle) ?amount-ben)
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
