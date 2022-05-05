(in-package :colitrsynth)

(defvar *app*)

(defclass app ()
  ((win :initarg :win :accessor .win)
   (width :initarg :width :initform 800 :accessor .width)
   (height :initarg :height :initform 600 :accessor .height)
   (font :initform nil :accessor .font)
   (modules :initarg :modules :initform '() :accessor .modules)))

(defclass renderable ()
  ((color :initarg :color :initform (list #xcc #xcc #xcc #xff) :accessor .color)
   (x :initarg :x :initform 10)
   (y :initarg :y :initform 10)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)
   (parent :initarg :parent :initform nil :accessor .parent)
   (children :initarg :children :initform nil :accessor .children)))

(defmethod add-child ((parent renderable) (child renderable))
  (push child (.children parent))
  (setf (.parent child) parent))

(defmethod .x ((self renderable))
  (+ (slot-value self 'x)
     (if (.parent self)
         (.x (.parent self))
         0)))

(defmethod .y ((self renderable))
  (+ (slot-value self 'y)
     (if (.parent self)
         (.y (.parent self))
         0)))

(defmethod (setf .x) ((self renderable) value)
  (setf (slot-value self 'x) value))

(defmethod (setf .y) ((self renderable) value)
  (setf (slot-value self 'y) value))

(defmethod render ((self renderable) renderer)
  (with-slots (color width height) self
    (sdl2:render-draw-rect renderer (sdl2:make-rect (.x self) (.y self) width height)))
  (loop for child in (.children self)
        do (render child renderer)))

(defclass module (renderable)
  ((name :initarg :name :initform "noname" :accessor .name)))

(defmethod initialize-instance :after ((self module) &key)
  (add-child self (make-instance 'text :value (.name self) :x 3 :y 3)))

(defclass text (renderable)
  ((value :initarg :value :initform "Hi" :accessor .value)))

(defmethod render ((self text) renderer)
  (let* ((surface  (apply #'sdl2-ttf:render-utf8-solid (.font *app*) (.value self) (.color self)))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (sdl2:render-copy renderer
                      texture
                      :source-rect nil
                      :dest-rect (sdl2:make-rect (.x self) (.y self) width height))))

(defun main ()
  (sb-thread:make-thread 'main-loop))

(defun main-loop ()
  (let ((*app* (setf *app* (make-instance
                            'app
                            :width 800
                            :height 600
                            :modules (list (make-instance 'module
                                                          :name "にゃ～"
                                                          :x 60
                                                          :y 50
                                                          :width 100
                                                          :height 80)
                                           (make-instance 'text
                                                          :value "Hello, こんにちは。"
                                                          :x 60
                                                          :y 150 ))))))
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
          (with-audio
            (let* ((out (.out *audio*))
                   (pattern1 (make-instance 'pattern
                                            :lines (list a4 e4 none g4
                                                         a4 off  g4 c4)))
                   (osc1 (make-instance 'sin-wave))
                   (adsr1 (make-instance 'adsr :d 0.2d0 :s 0d0))
                   (amp1 (make-instance 'amp))
                   (pattern2 (make-instance 'pattern
                                            :lines (list a3 e3 none g3
                                                         a3 off  g3 c3)))
                   (osc2 (make-instance 'saw-wave))
                   (adsr2 (make-instance 'adsr :d 0.7d0 :s 0.8d0))
                   (amp2 (make-instance 'amp)))
              (connect pattern1 osc1)
              (connect pattern1 adsr1)
              (connect osc1 amp1)
              (connect adsr1 amp1)
              (connect amp1 out)
              (connect pattern2 osc2)
              (connect pattern2 adsr2)
              (connect osc2 amp2)
              (connect adsr2 amp2)
              (connect amp2 out)
              (add-pattern (.sequencer *audio*) pattern1 0)
              (add-pattern (.sequencer *audio*) pattern1 (length (.lines pattern1)))
              (add-pattern (.sequencer *audio*) pattern2 (length (.lines pattern1)))
              (add-pattern (.sequencer *audio*) pattern1 (* 2 (length (.lines pattern1))))
              (add-pattern (.sequencer *audio*) pattern2 (* 2 (length (.lines pattern1))))
              (unwind-protect
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
                            t))
                (stop-audio)))))))))

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
          button state clicks x y)
  (if (.playing *audio*)
      (stop-audio)
      (play-audio)))

(defun idle (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 #xff)
  (sdl2:render-clear renderer)
  (sdl2:set-render-draw-color renderer #xcc #xcc #xcc #xff)

  (loop for module in (.modules *app*)
        do (render module renderer))
  
  (sdl2:render-present renderer)
  (when (.request-stop *audio*)
    (stop-audio))
  (sdl2:delay 50))                      ;ms
