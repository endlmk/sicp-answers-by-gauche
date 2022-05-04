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

