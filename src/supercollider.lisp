(in-package :colitrsynth)

(defvar *app*)

(defclass app ()
  ((win :initarg :win :accessor .win)
   (width :initarg :width :initform 800 :accessor .width)
   (height :initarg :height :initform 600 :accessor .height)
   (modules :initarg :modules :initform '() :accessor .modules)
   (super-collider-server
    :initarg :super-collider-server
    :accessor .super-collider-server)
   (bpm :initarg :bpm
        :initform 128.0
        :accessor .bpm)
   (dexed :accessor .dexed)))

(defmethod initialize-instance :after ((app app) &key)
  (sc:clock-bpm (.bpm app))
  (sc:defsynth dexed ()
    (sc:out.ar 0 (sc-vst:vst-plugin.ar nil 2 :dexed)))
  (setf (.dexed app)
        (sc-vst:vst-controller (sc:synth 'dexed) :dexed "Dexed.vst3")))

(defclass module ()
  ((name :initarg :name :initform 10 :accessor .name)
   (color :initarg :color :initform '(0.5 0.5 0.5) :accessor .color)
   (x :initarg :x :initform 10 :accessor .x)
   (y :initarg :y :initform 10 :accessor .y)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)))

(defmethod show ((module module))
  (with-slots (color x y width height) module
    (let* ((x1 x)
           (x2 (+ x1 width))
           (y1 y)
           (y2 (+ y1 height)))
      (gl:begin :triangles)
      (apply #'gl:color color)
      (gl:vertex x1 y2)
      (gl:vertex x1 y1)
      (gl:vertex x2 y2)
      (gl:vertex x2 y2)
      (gl:vertex x1 y1)
      (gl:vertex x2 y1)
      (gl:end))))

(defun main ()
  (sb-thread:make-thread 'main-loop))

(defun main-loop ()
  (let ((*app* (setf *app* (make-instance
                            'app
                            :width 800
                            :height 600
                            :super-collider-server (cl-patterns:start-backend :supercollider)
                            :modules (list (make-instance 'module
                                                          :x 60
                                                          :y 50
                                                          :width 100
                                                          :height 80))))))
    (sdl2:with-init (:everything)
      (format t "Using SDL Library Version: ~D.~D.~D~%"
              sdl2-ffi:+sdl-major-version+
              sdl2-ffi:+sdl-minor-version+
              sdl2-ffi:+sdl-patchlevel+)
      (finish-output)

      (sdl2:with-window (win :flags '(:opengl))
        (setf (.win *app*) win)
        (sdl2:with-gl-context (gl-context win)
          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:gl-make-current win gl-context)
          (gl:viewport 0 0 (.width *app*) (.height *app*))
          (gl:matrix-mode :projection)
          (gl:ortho 0 (.width *app*) (.height *app*) 0 -1.0 1.0)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer)

          (sdl2:hide-window win)
          (sdl2:show-window win)

          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)

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
    (1
     ;; (sc-vst:play-note (.dexed *app*) (sc:clock-quant 4) 1 60 80 2)
     (sc-vst:note-on (.dexed *app*) 1 60 80))
    (2 (sc-vst:note-off (.dexed *app*) 1 60 80))
    (3 (sc-vst:editor (.dexed *app*)))))

(defun mousebuttonup (button state clicks x y)
  (format t "Mouse button up button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y))

(defun idle ()
  (gl:clear :color-buffer)
  (loop for module in (.modules *app*)
        do (show module))
  (gl:flush)
  (sdl2:gl-swap-window (.win *app*))
  ;; sdl2:with-event-loop はスピンループのようで 1CPU が 100% 使用中になる
  ;; ゆっくりでいいときはスリープいれるといいかもしれない
  ;;(sleep 0.1)
  )
