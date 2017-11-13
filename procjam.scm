(define 2pi (* 2 pi))
(define pi/2 (/ pi 2))
(define pi/3 (/ pi 3))
(define pi/4 (/ pi 4))

(include "gen.scm")
(include "utils.scm")

(define base-color (random-color))

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
    (list center-color                     ;; center color
          (pseudo-random-real)             ;; center size
          poly                             ;; polygon vertices number
          (* 2pi (pseudo-random-real))     ;; skew
          (map random-petal-layer colors)) ;; petals layers
    ))

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

(define (hybridize section f1 f2)
  (let ()
    (map
      (lambda (i g1 g2)
        (cond ((color? g1)
               (apply color:L*C*h
                      (hybridize (pseudo-random-integer 3)
                                 (color->L*C*h g1)
                                 (color->L*C*h g2))))
              ((pair? g1)
               (let ((l1 (length g1))
                     (l2 (length g2)))
                 (cond ((> l1 l2)
                        (hybridize section
                                   g1
                                   (if (zero? (pseudo-random-integer 2))
                                       g2
                                       (append g2 g1)))
                        )
                       ((> l2 l1)
                        (hybridize section
                                   (if (zero? (pseudo-random-integer 2))
                                       g1
                                       (append g1 g2))
                                   g2)
                        )
                       (else
                         (hybridize section g1 g2)))))
              ((> i section)
               g1)
              (else
                g2))
        )
      (iota (length f1)) f1 f2)
      ))

(define (all-hybridizations f1 f2)
  (list-tabulate 7 (lambda (i) (hybridize i f1 f2))))

(set-pseudo-random-seed! (random-bytes))
#;(set-pseudo-random-seed! #${3c7ed09c3a86bb8c35cb8a35ee5a30a5e1fbc23b928c9596e605b54abc9dc09b3bf4d8feac0c97df97ec8f0602c3dc48cf6170d92a7485333a6798f1d1f2eef1a492a16a639141011ce1ac45923f0a83c962985d656e78292b12e96ee888185967151127bcd3f419d450a5e2e9921b0ebb9b7ae9e6076446fa13beb32d7420bf})

(define *flower* (random-flower))

(define (show-frame)
  (set! base-color (color:rotate-hue base-color (/ dt 10)))
  (apply set-source-rgb! ctx (color->cairo (color:L*C*h 50 0 0)))
  (paint! ctx)
  
  ;; Stream of flowers
  (let ()
    (save! ctx)
    (translate! ctx ww/2 wh/2)
    (scale! ctx 100 100)
    (apply flower! *flower*)
    (restore! ctx)))

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

(define (flower! center-color center-size poly skew layers)
  (save! ctx)
  (rotate! ctx skew)
  (for-each
    (lambda (p)
      (apply petals! poly p)
      (rotate! ctx (/ 2pi (* poly (length layers)))))
    layers)
  (center! center-size center-color poly)
  (restore! ctx)
  )
  
(define (center! size color n)
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

(define (petals! n color . args)
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
    (fill! ctx)
    (pattern-destroy! p)))

(define (petal i near far near-angle far-angle moment)
  (let* ((v (make-polar (+ 2
                           (* 1/50 (sin (* (add1 i) moment))))
                        (* 1/50 (cos (* (add1 i) moment)))))
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
    ))