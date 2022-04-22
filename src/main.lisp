(in-package :colitrsynth)

(defvar *app*)

(defclass app ()
  ((win :initarg :win :accessor .win)
   (super-collider-server
    :initarg :super-collider-server
    :accessor .super-collider-server)
   (dexed :accessor .dexed)))

(defmethod initialize-instance :after ((app app) &key)
  (sc:defsynth dexed ()
    (sc:out.ar 0 (sc-vst:vst-plugin.ar nil 2 :dexed)))
  (setf (.dexed app)
        (sc-vst:vst-controller (sc:synth 'dexed) :dexed "Dexed.vst3")))

(defun main ()
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:opengl))
      (sdl2:with-gl-context (gl-context win)
        ;; basic window/gl setup
        (format t "Setting up window/gl.~%")
        (finish-output)
        (sdl2:gl-make-current win gl-context)
        (gl:viewport 0 0 800 600)
        (gl:matrix-mode :projection)
        (gl:ortho -2 2 -2 2 -2 2)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
        (gl:clear-color 0.0 0.0 1.0 1.0)
        (gl:clear :color-buffer)

        (sdl2:hide-window win)
        (sdl2:show-window win)

        ;; main loop
        (format t "Beginning main loop.~%")
        (finish-output)

        (setf *app* (make-instance
                     'app
                     :win win
                     :super-collider-server (cl-patterns:start-backend :supercollider)))

        (let ((*app* *app*))
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
                      (keydown keysym))

            (:keyup (:keysym keysym)
                    (keyup keysym))

            (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                          (mousemotion x y xrel yrel state))

            (:mousebuttondown (:button button :state state :clicks clicks :x x :y y)
                              (mousebuttondown button state clicks x y))

            (:mousebuttonup (:button button :state state :clicks clicks :x x :y y)
                            (mousebuttonup button state clicks x y))

            (:idle ()
                   (idle))

            (:quit () t)))))))

(defun keydown (keysym)
  (let ((scancode (sdl2:scancode-value keysym))
        (sym (sdl2:sym-value keysym))
        (mod-value (sdl2:mod-value keysym)))
    (cond
      ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
      ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
      ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
            sym
            scancode
            mod-value)))

(defun keyup (keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit)))

(defun mousemotion (x y xrel yrel state)
  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
          x xrel y yrel state))

(defun mousebuttondown (button state clicks x y)
  (format t "Mouse button down button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (case button
    (1 (sc-vst:note-on (.dexed *app*) 1 60 80))
    (2 (sc-vst:note-off (.dexed *app*) 1 60 80))
    (3 (sc-vst:editor (.dexed *app*)))))

(defun mousebuttonup (button state clicks x y)
  (format t "Mouse button up button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y))

(defun idle ()
  (gl:clear :color-buffer)
  (gl:begin :triangles)
  (gl:color 1.0 0.5 0.5)
  (gl:vertex 1.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush)
  (sdl2:gl-swap-window (.win *app*))
  ;; sdl2:with-event-loop はスピンループのようで 1CPU が 100% 使用中になる
  ;; ゆっくりでいいときはスリープいれるといいかもしれない
  ;;(sleep 0.1)
  )
