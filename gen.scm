(define (random-elt l)
  (list-ref l (random (length l))))

(define (either . proc)
  ((random-elt proc)))

(define (until pred proc)
  (let ((v (proc)))
    (if (pred v)
        v
        (until pred proc))))
  
(define (gen-op)
  (random-elt '(+ - *)))

(define (gen-integer)
  ((random-elt (list + -)) (random 100)))

(define ((make-gen-fraction g))
  (/ (g) (until (o not zero?) g)))

(define (gen-float)
  (exact->inexact ((make-gen-fraction gen-integer))))

(define (gen-real)
  (either gen-integer (make-gen-fraction gen-integer) gen-float))

(define (gen-complex)
  (either gen-rectangular gen-polar))

(define (gen-rectangular)
  (make-rectangular (gen-real) (gen-real)))

(define (gen-polar)
  (make-polar (gen-real) (gen-real)))

(define (gen-number)
  (either gen-real gen-complex (make-gen-fraction gen-complex)))

(define (gen-expr)
  `(,(gen-op) ,(either gen-expr gen-number) ,(either gen-expr gen-number)))
