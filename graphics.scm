(void)

(define bee-delay 10000)
(define bee-wait-delay 5000)

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


;; movo-to: m
;; line-to: l
;; curve-to: c
;; close: /
(define (path desc)
  (unless (null? desc)
    (let ((proc (car desc))
          (rest (cdr desc)))
      (case proc
        ((/)
         (close-path! ctx)
         (path rest))
        ((m)
         (apply move-to! ctx (take rest 2))
         (path (drop rest 2)))
        ((l)
         (apply line-to! ctx (take rest 2))
         (path (drop rest 2)))
        ((c)
         (apply curve-to! ctx
                (take rest 6))
         (path (drop rest 6)))))))

(define f (bezier #f64(200 200) #f64(0 0) #f64(0 0) #f64(38 92)))

(define (tunnel)
  (let ((divs 400)
        (freq 20)
        (amp 100))
    (dotimes (i (add1 divs))
             (let* ((a (+ (* 2 pi (* freq (/ i divs))) (/ now 1000)))
                    (s (sin a))
                    (c (cos a))
                    (x (* ww (/ i divs)))
                    (y (+ (quotient wh 2)
 (* amp s)))
                    (as (+ c 1.6)))
               (circle x y (* 2 as))
               (fill! ctx)))))

(define (sine freq)
  (let ((divs 500)
        (amp 100))
    (dotimes (i (add1 divs))
             (let* ((a (* 2 pi (* freq (/ i divs))))
                    (s (sin a))
                    (x (* ww (/ i divs)))
                    (y (+ (quotient wh 2)
                          (* amp s))))
               (line-to! ctx x y)))
    (stroke! ctx)))

(define (flower angle scale x y)
  (save! ctx)
  (translate! ctx x y)
  (rotate! ctx angle)
  (scale! ctx scale scale)
  (circle 10 0 10)
  (circle 0 10 10)
  (circle -10 0 10)
  (circle 0 -10 10)
  (set-source-rgba! ctx (/ 82 255) (/ 105 255) (/ 193 255) 1)
  (fill! ctx)
  (circle 0 0 7)
  (set-source-rgb! ctx (/ 128 255) (/ 218 255) (/ 56 255))
  (fill! ctx)
  (restore! ctx))

(define flowers '())
(define (genfield n)
  (dotimes (i n)
           (push! (list (+ 1000 (random 10000))
                        (+ 500 (random 1000))
                        (+ 40 (random (- ww 80)))
                        (+ 40 (random (- wh 80))))
                  flowers)))

(genfield 50)

(define (bee-trajectory f1 f2)
  (let* ((x1 (third f1))
         (y1 (fourth f1))
         (x2 (third f2))
         (y2 (fourth f2))
         (c1x (+ x1 (- 150 (random 300))))
         (c1y (+ y1 (- 150 (random 300))))
         (c2x (+ x2 (- 150 (random 300))))
         (c2y (+ y2 (- 150 (random 300)))))
    (set! *bee-curve*
      (lambda ()
        (move-to! ctx x1 y1)
        (curve-to! ctx c1x c1y c2x c2y x2 y2)
        (set-source-rgb! ctx 0 0 0)
        (stroke! ctx)))
    (set! f (bezier (f64vector x1 y1)
                    (f64vector c1x c1y)
                    (f64vector c2x c2y)
                    (f64vector x2 y2)))))

(define *bee-curve* '())
(define *bee-destination* (second flowers))
(define *next* (+ now 2000))
(bee-trajectory (first flowers) *bee-destination*)

(define (bee x y angle)
  (save! ctx)
  (translate! ctx x y)
  #;(rotate! ctx angle)
  (scale! ctx 0.8 0.8)
  
  ;; wings
  (rotate! ctx angle)
  (set-source-rgba! ctx (/ 158 255) 1 (/ 249 255) 0.8)
  (circle -11 0 7)
  (circle 11 0 7)
  (fill! ctx)
  (rotate! ctx (- angle))
  
  ;; body
  (rotate! ctx (/ (- angle) 100))
  (set-source-rgb! ctx (/ 244 255) (/ 235 255) 0)
  (circle 0 0 10)
  (fill-preserve! ctx)
  (clip! ctx)
  
  ;; markings
  (set-line-width! ctx 4)
  (set-source-rgb! ctx (/ 134 255) (/ 120 255) 0)
  (move-to! ctx -10 -4.5)
  (line-to! ctx 10 -4.5)
  (stroke! ctx)
  (move-to! ctx -10 4.5)
  (line-to! ctx 10 4.5)
  (stroke! ctx)
  (restore! ctx))

(define truc-fun quadratic-ease)
(define (truc t)
  (if (<= t 0.5)
      (tween truc-fun 'in 1 0 (* t 2))
      (tween truc-fun 'out 0 1 (* (- t 0.5) 2))))

(define (show-frame)
  (handle-network)
  
  (set-source-rgb! ctx 0.72 0.51 0.65)
  (paint! ctx)

  #;(let* ((v (f (+ 0.5 (* 0.5 (sin (/ now 1000))))))
         (x (f64vector-ref v 0))
         (y (f64vector-ref v 1)))
    (path '(m 200 200 c 0 0 0 0 38 92))
    (set-source-rgb! ctx 1 1 0)
    (stroke! ctx)
    (circle x y 40)
    (set-source-rgb! ctx 1 0 0)
    (fill! ctx)
    (circle x y
            (* 40 (/ (modulo now 500) 500))))

  #;(begin
    (set-source-rgb! ctx 0 0.3 0.40)
    (tunnel))
  
  #;(begin
    (flower (/ now 10000) (+ 0.5 (abs (sin (/ now 500)))) 300 300)
    (flower (/ now 2000) 2 400 400))
  
  (set-source-rgb! ctx 0 0 0)
  #;(sine (/ *pd-freq* 10))
  
  (for-each
    (lambda (f)
      (flower (/ now (- (first f) (* *pd-freq* 10)))
              (+ 0.5 (abs (sin (/ now (second f)))))
              (third f)
              (fourth f)))
    flowers)
  
  (let* ((s (clip (- 1 (/ (- *next* now) bee-delay)) 0 1))
         (rot (truc s))
         (v (f (tween sinusoidal-ease 'inout 0 1 s)))
         (x (f64vector-ref v 0))
         (y (f64vector-ref v 1)))
    #;(*bee-curve*)
    (bee x y (/ now (* (if (zero? rot) 1 (* 0.5 rot)) 3000)))
    (when (>= now *next*)
      (set! f (lambda (t) (f64vector (third *bee-destination*) (fourth *bee-destination*))))
      (when (>= now (+ *next* bee-wait-delay))
        (let ((prev *bee-destination*))
          (set! *bee-destination* (list-ref (delete prev flowers) (random (sub1 (length flowers)))))
          (set! *next* (+ now bee-delay))
          (bee-trajectory prev *bee-destination*)))))
  )

(define *pd-freq* 1)
(define (handle-network)
  (when (char-ready? pdin)
    (set! *pd-freq*
      (string->number (car (string-split (read-line pdin) ";"))))))