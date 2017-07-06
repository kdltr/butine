(void)

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

 
 
 
(define (show-frame)
  (let* ((v (f (+ 0.5 (* 0.5 (sin (/ now 1000))))))
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

  (begin
    (set-source-rgb! ctx 0.5 0.3 0.40)
    (tunnel))
  
  (begin
    (flower (/ now 10000) (+ 0.5 (abs (sin (/ now 500)))) 300 300)
    (flower (/ now 2000) 2 400 400))
  
  (begin
    (set-source-rgb! ctx 0 0 0)
    (sine (/ *pd-freq* 10)))
  )