(use (prefix sdl2 sdl2:) cairo cairo.image)

(define ww 32)
(define wh 32)

(sdl2:set-hint! 'render-scale-quality "0")
(sdl2:set-main-ready!)
(sdl2:init! '(video))
(define window (sdl2:create-window! "Re-ception" 0 0 1024 1024))

(define renderer (sdl2:create-renderer! window -1 '(accelerated)))
(set! (sdl2:render-logical-size renderer) (list ww wh))

(define s (image-surface-create +format-rgb24+ ww wh))
(define c (create s))
(set-antialias! c +antialias-none+)
(define t (sdl2:create-texture renderer 'rgb888 'streaming ww wh))

(define (update-texture! t s)
  (surface-flush! s)
  (sdl2:update-texture-raw! t #f (image-surface-get-data s) (image-surface-get-stride s)))

(define (draw-frame)
  (set-source-rgb! c 0 0 0)
  (paint! c)
  
  (save! c)
  (translate! c
              (+ (/ ww 2) (* 8 (sin (/ now 1000))))
              (+ (/ wh 2) (* 8 (cos (/ now 1000)))))
  (rotate! c (/ now 250))
  (rectangle! c -4 -4 8 8)
  (set-source-rgb! c 1 0 0)
  (fill! c)
  (restore! c))

(define (main-loop)
  (set! now (sdl2:get-ticks))
  (draw-frame)
  (update-texture! t s)
  (sdl2:render-clear! renderer)
  (sdl2:render-copy! renderer t)
  (sdl2:render-present! renderer)
  (main-loop))

(main-loop)