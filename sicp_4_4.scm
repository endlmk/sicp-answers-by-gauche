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