
(define (circle x y r)
  (arc! ctx x y r 0 (* 2 pi)))

(define (clip x min max)
  (if (< x min) min (if (> x max) max x)))

(define (vec* v1 v2)
  (let ((v1 (if (number? v1) (f64vector v1 v1) v1))
        (v2 (if (number? v2) (f64vector v2 v2) v2)))
    (f64vector (* (f64vector-ref v1 0)
                  (f64vector-ref v2 0))
               (* (f64vector-ref v1 1)
                  (f64vector-ref v2 1)))))

(define (vec+ v1 v2)
  (f64vector (+ (f64vector-ref v1 0)
                (f64vector-ref v2 0))
             (+ (f64vector-ref v1 1)
                (f64vector-ref v2 1))))

(define (bezier p0 p1 p2 p3)
  (lambda (t)
    (vec+ (vec+ (vec* p0 (expt (- 1 t) 3))
                (vec* 3 (vec* p1 (* t (expt (- 1 t) 2)))))
          (vec+ (vec* 3 (vec* p2 (* (expt t 2) (- 1 t))))
                (vec* p3 (expt t 3))))))
