(import scheme chicken extras data-structures srfi-1)
(use (prefix sdl2 sdl2:)
     cairo cairo.surface.image cairo.surface.svg miscmacros srfi-4
     new-random
     color numbers)


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

(define now (sdl2:get-ticks))
(define dt 0)
(define quit? #f)

(include "procjam.scm")

(define (handle-events)
  (let ((e (sdl2:poll-event!)))
    (when e
      (handle-event e)
      (handle-events))))

(define (handle-event e)
  (case (sdl2:event-type e)
    ((key-down)
     (handle-key-down-event e))
    ((quit)
     (set! quit? #t))))

(define (handle-key-down-event e)
  (case (sdl2:keyboard-event-scancode e)
    ((escape) (set! quit? #t))
    ((space) (set! *flower* (random-flower)))
    ((return) (screenshot!))))

(define (screenshot!)
  (let* ((filename (sprintf "flower-~A.svg"
                            (time->string (seconds->local-time)
                                          "%Y%m%d-%H%M%S")))
         (surface (svg-surface-create filename ww wh))
         (context (create surface)))
    (fluid-let ((ctx context))
      (show-frame))
    (destroy! context)
    (surface-destroy! surface)))

(let loop ()
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
  (handle-events)
  (unless quit?
    (loop)))

