(import scheme chicken extras data-structures srfi-1)
(use (prefix sdl2 sdl2:)
     cairo cairo.surface.image posix tcp miscmacros srfi-4
     new-random tween)

(set-signal-handler! signal/int exit)
(set-signal-handler! signal/term exit)

(define graphics-file (car (command-line-arguments)))

#;(define-values (pdin pdout) (tcp-connect "localhost" 1234))

(define ww 800)
(define wh 480)

(sdl2:set-hint! 'render-scale-quality "0")
(sdl2:set-main-ready!)
(sdl2:init! '(video))
(define window (sdl2:create-window! "Butine" 0 0 800 480))
(define render (sdl2:create-renderer! window -1 '(accelerated)))
(set! (sdl2:render-logical-size render) (list 800 480))

(define s (image-surface-create 'rgb24 ww wh))
(define ctx (create s))
(define t (sdl2:create-texture render 'rgb888 'streaming ww wh))

(define (update-texture! t s)
  (surface-flush! s)
  (sdl2:update-texture-raw! t #f (image-surface-get-data s) (image-surface-get-stride s)))

(define-syntax safe
  (syntax-rules ()
    ((safe body)
     (handle-exceptions exn
       (begin
         (set! dirty #t)
         (print-error-message exn)
         (print-call-chain))
       body
       (set! dirty #f)))))


(define file-mod (file-modification-time graphics-file))
(define now (sdl2:get-ticks))
(define dt 0)
(define dirty #f)

(load graphics-file)

(define (reload-graphics)
  (let ((new-mod (file-modification-time graphics-file)))
    (when (> new-mod file-mod)
      (safe (load graphics-file)))
    (set! file-mod new-mod)))

(let loop ()
  (reload-graphics)
  (let ((t (sdl2:get-ticks)))
    (set! dt (- t now))
    (set! now t))
  (save! ctx)
  (unless dirty
    (safe (show-frame)))
  (restore! ctx)
  (update-texture! t s)
  (sdl2:render-clear! render)
  (sdl2:render-copy! render t)
  (sdl2:render-present! render)
  (loop))

