(use (prefix sdl2 sdl2:)
     cairo cairo.image posix tcp miscmacros srfi-4 tween)

(define-values (pdin pdout) (tcp-connect "localhost" 1234))

(define ww 800)
(define wh 480)

(sdl2:set-hint! 'render-scale-quality "0")
(sdl2:set-main-ready!)
(sdl2:init! '(video))
(define window (sdl2:create-window! "Re-ception" 0 0 800 480))
(define render (sdl2:create-renderer! window -1 '(accelerated)))
(set! (sdl2:render-logical-size render) (list 800 480))

(define s (image-surface-create +format-rgb24+ ww wh))
(define ctx (create s))
(define t (sdl2:create-texture render 'rgb888 'streaming ww wh))

(define (update-texture! t s)
  (surface-flush! s)
  (sdl2:update-texture-raw! t #f (image-surface-get-data s) (image-surface-get-stride s)))


(define file-mod (file-modification-time "graphics.scm"))
(define now (sdl2:get-ticks))
(define dt 0)

(load "graphics.scm")

(let loop ()
  (let ((new-mod (file-modification-time "graphics.scm")))
    (when (> new-mod file-mod)
      (load "graphics.scm"))
    (set! file-mod new-mod))
  (let ((t (sdl2:get-ticks)))
    (set! dt (- t now))
    (set! now t))
  (save! ctx)
  (show-frame)
  (restore! ctx)
  (update-texture! t s)
  (sdl2:render-clear! render)
  (sdl2:render-copy! render t)
  (sdl2:render-present! render)
  (loop))
