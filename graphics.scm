(include "utils.scm")

(define bee-delay 10000)
(define bee-wait-delay 3000)

(define f (bezier #f64(200 200) #f64(0 0) #f64(0 0) #f64(38 92)))

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
           (push! (list (+ 1000 (pseudo-random-integer 10000))
                        (+ 500 (pseudo-random-integer 1000))
                        (+ 40 (pseudo-random-integer (- ww 80)))
                        (+ 40 (pseudo-random-integer (- wh 80))))
                  flowers)))

(genfield 100)

(define (choose-flower x y)
  (let ((flowers (remove (lambda (f) (and (= (third f) x)
                                          (= (fourth f) y)))
                         flowers)))
    (list-ref flowers (pseudo-random-integer (length flowers)))))
         

(define (bee-trajectory x1 y1 x2 y2)
  (let* ((c1x (+ x1 (- 150 (pseudo-random-integer 300))))
         (c1y (+ y1 (- 150 (pseudo-random-integer 300))))
         (c2x (+ x2 (- 150 (pseudo-random-integer 300))))
         (c2y (+ y2 (- 150 (pseudo-random-integer 300)))))
    (set! *bee-curve*
      (lambda ()
        (move-to! ctx x1 y1)
        (curve-to! ctx c1x c1y c2x c2y x2 y2)
        (set-source-rgb! ctx 0 0 0)
        (stroke! ctx)))
    (bezier (f64vector x1 y1)
            (f64vector c1x c1y)
            (f64vector c2x c2y)
            (f64vector x2 y2))))

(define *bee-curve* '())

(define (draw-bee x y angle)
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

(define (make-bee)
  (let ()
    (define current-state) ;; assigned later
    (define bx (/ ww 2))
    (define by (/ wh 2))
    (define angle 0)
    
    (define (run-state)
      (current-state)
      (draw-bee bx by angle))
  
    ;; STATES
    
    (define (wait t get-next-state)
      (let ((next-time (+ t now)))
        (lambda ()
          (when (>= now next-time)
            (set! current-state (get-next-state))))))
    
    (define (wander)
      (lambda ()
        (set! bx (+ bx (- 1 (pseudo-random-integer 3))))
        (set! by (+ by (- 1 (pseudo-random-integer 3))))))

    (define (fly-to destx desty)
      (let ((destination-time (+ now (pseudo-random-integer bee-delay)))
            (*next* (+ now bee-delay))
            (trajectory (bee-trajectory bx by destx desty))
            )
        (lambda ()
          (let* ((s (clip (- 1 (/ (- *next* now) bee-delay)) 0 1))
                 (rot (truc s))
                 (v (trajectory (tween sinusoidal-ease 'inout 0 1 s)))
                 (x (f64vector-ref v 0))
                 (y (f64vector-ref v 1)))
            (set! bx x)
            (set! by y)
            (set! angle (+ angle (* (/ pi 64) dt)))
            (when (>= now *next*)
              (let* ((next-flower (choose-flower bx by))
                     (fx (third next-flower))
                     (fy (fourth next-flower)))
                (set! current-state
                  (wait (pseudo-random-integer bee-wait-delay)
                        (lambda () (fly-to fx fy))))))))
        ))
    
    (let ((first-flower (choose-flower (/ ww 2) (/ wh 2))))
      (set! current-state (fly-to (third first-flower) (fourth first-flower)))) ;; initial state
    run-state))

(define bees (list-tabulate 40 (lambda (i) (make-bee))))

(define (show-frame)
  #;(handle-network)
  
  (set-source-rgb! ctx 0.72 0.51 0.65)
  (paint! ctx)

  (for-each
    (lambda (f)
      (flower (/ now (- (first f) (* *pd-freq* 10)))
              (+ 0.5 (abs (sin (/ now (second f)))))
              (third f)
              (fourth f)))
    flowers)
  
  (for-each (lambda (b) (b)) bees)
  )

(define *pd-freq* 1)

#;(define (handle-network)
  (when (char-ready? pdin)
    (set! *pd-freq*
      (string->number (car (string-split (read-line pdin) ";"))))))