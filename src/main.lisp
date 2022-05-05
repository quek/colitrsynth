(in-package :colitrsynth)

(defvar *app*)

(defclass app ()
  ((win :initarg :win :accessor .win)
   (width :initarg :width :initform 800 :accessor .width)
   (height :initarg :height :initform 600 :accessor .height)
   (font :initform nil :accessor .font)
   (modules :initarg :modules :initform '() :accessor .modules)))

(defclass module ()
  ((name :initarg :name :initform 10 :accessor .name)
   (color :initarg :color :initform '(0.5 0.5 0.5) :accessor .color)
   (x :initarg :x :initform 10 :accessor .x)
   (y :initarg :y :initform 10 :accessor .y)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)))

(defmethod render ((module module) renderer)
  (with-slots (color x y width height) module
    (sdl2:render-draw-rect renderer (sdl2:make-rect x y width height))))

(defun main ()
  (sb-thread:make-thread 'main-loop))

(defun main-loop ()
  (let ((*app* (setf *app* (make-instance
                            'app
                            :width 800
                            :height 600
                            :modules (list (make-instance 'module
                                                          :x 60
                                                          :y 50
                                                          :width 100
                                                          :height 80))))))
    (sdl2:with-init (:everything)
      (sdl2-ttf:init)
      (let ((font "c:/Windows/Fonts/msgothic.ttc"))
        (format t "Load font ~a~%" font)
        (setf (.font *app*)
              (sdl2-ttf:open-font
               font
               10)))

      (format t "Using SDL Library Version: ~D.~D.~D~%"
              sdl2-ffi:+sdl-major-version+
              sdl2-ffi:+sdl-minor-version+
              sdl2-ffi:+sdl-patchlevel+)
      (finish-output)

      (sdl2:with-window (win :title "CoLiTrSynth" :w (.width *app*) :h (.height *app*))
        (setf (.win *app*) win)
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:hide-window win)
          (sdl2:show-window win)
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
                   (idle renderer))

            (:quit ()
                   (when (.font *app*)
                     (print 'sdl2-ttf:close-font)
                     (sdl2-ttf:close-font (.font *app*))
                     (setf (.font *app*) nil))
                   (when (= 1 (sdl2-ttf:was-init))
                     (print 'sdl2-ttf:quit)
                     (sdl2-ttf:quit))
                   t)))))))

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
          button state clicks x y))

(defun mousebuttonup (button state clicks x y)
  (format t "Mouse button up button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y))

(defun idle (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 #xff)
  (sdl2:render-clear renderer)
  (sdl2:set-render-draw-color renderer #xcc #xcc #xcc #xff)

  (loop for module in (.modules *app*)
        do (render module renderer))

  (let* ((surface  (sdl2-ttf:render-utf8-solid (.font *app*) "Hello World! こんにちは。" #xcc #xcc #xcc 0))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (sdl2:render-copy renderer
                      texture
                      :source-rect nil
                      :dest-rect (sdl2:make-rect 50 150 width height)))
  
  (sdl2:render-present renderer)
  (sdl2:delay 50))                      ;ms
