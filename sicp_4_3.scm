;;ex4.35
(define (an-integer-between l h)
  (require (<= l h))
  (amb l (an-integer-between (+ l 1) h)))
