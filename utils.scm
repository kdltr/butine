
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

(define (color->cairo c)
  (map (lambda (x) (exact->inexact (/ x 255))) (color->sRGB c)))

(define (color:rotate-hue color degrees)
  (let* ((LCh (color->L*C*h color))
         (hue (+ (caddr LCh) degrees)))
        (L*C*h->color
          (append (take LCh 2)
                  (list (cond ((< hue 0.0)
                               (let incr ((h hue))
                                 (if (>= h 0.0) h (incr (+ 360.0 h)))))
                              ((> hue 360.0)
                               (let reduce ((h hue))
                                 (if (<= h 360.0) h (reduce (- h 360.0)))))
                              (else
                                hue)))))))

(define (color:scale-chroma color factor)
  (let* ((LCh (color->L*C*h color))
                 (chroma (* (cadr LCh) factor)))
        (L*C*h->color (cons (car LCh) (cons chroma (cddr LCh))))))

(define (color:triad c)
  (list (color:rotate-hue c -120) c (color:rotate-hue c 120)))

(define (color:complement c)
  (color:rotate-hue c 180))

(define (random-color)
  (color:sRGB (pseudo-random-integer 255)
              (pseudo-random-integer 255)
              (pseudo-random-integer 255)))

(define (vec-curve-to! ctx v1 v2 v3)
  (curve-to! ctx
             (real-part v1) (imag-part v1)
             (real-part v2) (imag-part v2)
             (real-part v3) (imag-part v3)))