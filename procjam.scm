(use color cairo miscmacros numbers)

(define 2pi (* 2 pi))
(define pi/2 (/ pi 2))
(define pi/3 (/ pi 3))
(define pi/4 (/ pi 4))

(load "gen.scm")
(load "utils.scm")

(define base-color (random-color))
(print (color->cairo base-color))

(define ww/3 (round (/ ww 3)))
(define ww/2 (round (/ ww 2)))
(define wh/2 (round (/ wh 2)))

(define (random-flower)
  (let* ((center-color (color:L*C*h (between 70 100)
                                    (between 80 100)
                                    (between 0 360)))
         (layers (between 1 4))
         (poly (between 3 8))
         (comp (color:complement center-color))
         (a (between 20 30))
         (colors (cond ((= layers 1)
                        (list comp))
                       ((= layers 2)
                        (list (color:rotate-hue comp a)
                              (color:rotate-hue comp (- a))))
                       ((= layers 3)
                        (list (color:rotate-hue comp a)
                              comp
                              (color:rotate-hue comp (- a))))
                       ((= layers 4)
                        (list (color:rotate-hue comp (* a 2))
                              (color:rotate-hue comp a)
                              (color:rotate-hue comp (- a))
                              (color:rotate-hue comp (* 2 (- a)))))))
         )
  `((center-color . ,center-color)
    (center-size . ,(pseudo-random-real))
    (poly . ,poly)
    (layers . ,layers)
    (angle . ,(* 2pi (pseudo-random-real)))
    (petals ,@(map random-petal-layer colors))
    )))

(define (random-petal-layer color)
  (let* ((angle-div (between 1 5))
         (angle-mul (* 2 (pseudo-random-real)))
         (len-div1 (between 3 8))
         (len-num1 (between 1 (add1 len-div1)))
         (len-div2 (between 3 8))
         (len-num2 (between 1 (add1 len-div2)))
                   )
    (list ;; petal base color
          color
          ;; first control point magnitude
          (/ len-num1 len-div1)
          ;; second control point magnitude
          (/ len-num2 len-div2)
          ;; first control point angle
          (/ pi angle-div)
          ;; second control point angle
          (/ pi (* angle-mul angle-div))
          ;; displacement
          (* 2pi (pseudo-random-real)))
    ))

#;(set-pseudo-random-seed! (random-bytes))
(set-pseudo-random-seed! #${3c7ed09c3a86bb8c35cb8a35ee5a30a5e1fbc23b928c9596e605b54abc9dc09b3bf4d8feac0c97df97ec8f0602c3dc48cf6170d92a7485333a6798f1d1f2eef1a492a16a639141011ce1ac45923f0a83c962985d656e78292b12e96ee888185967151127bcd3f419d450a5e2e9921b0ebb9b7ae9e6076446fa13beb32d7420bf})
(define *flowers*
  (list-tabulate 40 (lambda (i) (cons i (random-flower)))))

(define (show-frame)
  (set! base-color (color:rotate-hue base-color (/ dt 10)))
  (apply set-source-rgb! ctx (color->cairo (color:L*C*h 50 0 0)))
  (paint! ctx)
  
  #;(let ((triad1 (color:triad base-color))
        (triad2 (color:triad (color:complement base-color))))
    (save! ctx)
    (color-line! triad1)
    (translate! ctx 0 wh/2)
    (color-line! triad2)
    (restore! ctx))
  
  ;; 40 flowers
  (for-each
    (lambda (i+f)
      (save! ctx)
      (let* ((i (car i+f))
             (f (cdr i+f))
             (x (modulo i 10))
             (y (quotient i 10)))
        (translate! ctx
                    (+ (/ ww 20) (* x (/ ww 10)))
                    (+ (/ wh 8) (* y (/ wh 4))))
        (scale! ctx 1.5 1.5)
        #;(rotate! ctx (alist-ref 'angle f))
        (flower! f))
    (restore! ctx))
    *flowers*)
  
  ;; Flower number n
  #;(let ((n 11))
      (save! ctx)
      (translate! ctx ww/2 wh/2)
      (scale! ctx 10 10)
      (flower! (cdr (list-ref *flowers* n)))
      (restore! ctx))
  
  ;; Stream of flowers
  #;(let ()
      (save! ctx)
      (translate! ctx ww/2 wh/2)
      (scale! ctx 10 10)
      (flower! (random-flower))
      (restore! ctx)
      (thread-sleep! 0.5))

  )

(define (color-line! colors)
  (save! ctx)
  (for-each
    (lambda (c)
      (apply set-source-rgb! ctx (color->cairo c))
      (rectangle! ctx 0 0 ww/3 wh/2)
      (fill! ctx)
      (translate! ctx ww/3 0))
    colors)
  (restore! ctx))

(define (flower! fl)
  (let ((poly (alist-ref 'poly fl))
        (layers (alist-ref 'layers fl))
        (pts (alist-ref 'petals fl)))
    (scale! ctx 10 10)
    
    (for-each
      (lambda (p)
        (apply petals poly p)
        (rotate! ctx (/ 2pi (* poly layers))))
      pts)
    
    (center (alist-ref 'center-size fl)
            (alist-ref 'center-color fl)
            poly)
  ))
  
(define (center size color n)
  (let ((angle (/ 2pi n))
        (base (make-polar size 0))
        (c (color->L*C*h color)))
    (dotimes (i (add1 n))
      (let ((endpt (make-polar (magnitude base) (* i angle))))
        (curve-to! ctx 0 0 0 0 (real-part endpt) (imag-part endpt))))
    (apply set-source-rgb! ctx (color->cairo color))
    (fill! ctx)
    (rotate! ctx (/ pi n))
    (dotimes (i n)
      (let ((endpt (make-polar (magnitude base) (* i angle))))
        (circle (real-part endpt) (imag-part endpt) 0.01)
        (fill! ctx))
             )
    ))

(define (petals n color . args)
  (let ((p (pattern-create-radial 0 0 0 0 0 2)))
    (apply pattern-add-color-stop-rgb!
           p 0.1 (color->cairo (color:scale-chroma color 0.3)))
    (apply pattern-add-color-stop-rgba!
           p 1 (append (color->cairo color)
                       '(0.9)))
    (set-source! ctx p)
    (dotimes (i n)
      (rotate! ctx (/ 2pi n))
      (apply petal i args))
    (pattern-destroy! p)))

(define (petal i near far near-angle far-angle moment)
  (let* ((v (make-polar (+ 2
                           (* 1/100 (sin (* (add1 i) moment))))
                        (* 1/100 (cos (* (add1 i) moment)))))
         (v2 (make-polar (* (magnitude v) near)
                         (+ (angle v) near-angle)))
         (v3 (make-polar (* (magnitude v) far)
                         (+ (angle v) far-angle)))
         (v4 (make-polar (* (magnitude v) near)
                         (- (angle v) near-angle)))
         (v5 (make-polar (* (magnitude v) far)
                         (- (angle v) far-angle))))
    (move-to! ctx 0 0)
    (vec-curve-to! ctx v2 v3 v)
    (vec-curve-to! ctx v5 v4 0)
    (fill! ctx)
    ))