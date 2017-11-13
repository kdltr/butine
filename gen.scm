(define (random-elt l)
  (list-ref l (pseudo-random-integer (length l))))

(define (either . proc)
  ((random-elt proc)))
  
(define (between a b)
  (+ a (pseudo-random-integer (- b a -1))))
