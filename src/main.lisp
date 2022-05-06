(in-package :colitrsynth)

(defparameter *transparency* #xc0)

(defvar *app*)

(defclass app ()
  ((win :initarg :win :accessor .win)
   (width :initarg :width :initform 800 :accessor .width)
   (height :initarg :height :initform 600 :accessor .height)
   (font :initform nil :accessor .font)
   (modules :initarg :modules :initform '() :accessor .modules)
   (mouse-x :initform 0 :accessor .mouse-x)
   (mouse-y :initform 0 :accessor .mouse-y)
   (mouse-left-down :initform nil :accessor .mouse-left-down)
   (drag-module :initform nil :accessor .drag-module)
   (connect-from-module :initform nil :accessor .connect-from-module)))

(defun module-at-mouse (app)
  (loop for module in (.modules app)
          thereis (and (<= (.x module) (.mouse-x app) (+ (.x module) (.width module)))
                       (<= (.y module) (.mouse-y app) (+ (.y module) (.height module)))
                       module)))

(defclass renderable ()
  ((color :initarg :color :initform (list #xdd #xdd #xdd *transparency*) :accessor .color)
   (x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)
   (parent :initarg :parent :initform nil :accessor .parent)
   (children :initarg :children :initform nil :accessor .children)))

(defmethod add-child ((parent renderable) (child renderable))
  (push child (.children parent))
  (setf (.parent child) parent))

(defmethod remove-child ((parent renderable) (child renderable))
  (setf (.children parent) (remove child (.children parent)))
  (setf (.parent child) nil))

(defmethod .x ((self null))
  0)

(defmethod .x ((self renderable))
  (+ (slot-value self 'x)
     (.x (.parent self))))

(defmethod .y ((self null))
  0)

(defmethod .y ((self renderable))
  (+ (slot-value self 'y)
     (.y (.parent self))))

(defmethod (setf .x) (value (self renderable))
  (setf (slot-value self 'x) value))

(defmethod (setf .y) (value (self renderable))
  (setf (slot-value self 'y) value))

(defmethod render ((self renderable) renderer)
  (with-slots (color width height) self
    (apply #'sdl2:set-render-draw-color renderer color)
    (sdl2:render-draw-rect renderer (sdl2:make-rect (.x self) (.y self) width height)))
  (loop for child in (.children self)
        do (render child renderer)))

(defmethod render ((self audio-module) renderer)
  (sdl2:set-render-draw-color renderer #x22 #x8b #x22 *transparency*)
  (loop for out in (.out self)
        do (let (x1 y1 x2 y2)
             (if (> (abs (- (.x self) (.x out)))
                    (abs (- (.y self) (.y out))))
                 (if (< (.x self) (.x out))
                     (if (< (.y self) (.y out))
                         (progn
                           (setf x1 (+ (.x self) (.width self)))
                           (setf y1 (+ (.y self) (/ (.height self) 2)))
                           (setf x2 (.x out))
                           (setf y2 (+ (.y out) (/ (.height out) 2))))
                         (progn
                           (setf x1 (+ (.x self) (.width self)))
                           (setf y1 (+ (.y self) (/ (.height self) 2)))
                           (setf x2 (.x out))
                           (setf y2 (+ (.y out) (/ (.height out) 2)))))
                     (if (< (.y self) (.y out))
                         (progn
                           (setf x1 (.x self))
                           (setf y1 (+ (.y self) (/ (.height self) 2)))
                           (setf x2 (+ (.x out) (.width out)))
                           (setf y2 (+ (.y out) (/ (.height out) 2))))
                         (progn
                           (setf x1 (.x self))
                           (setf y1 (+ (.y self) (/ (.height self) 2)))
                           (setf x2 (+ (.x out) (.width out)))
                           (setf y2 (+ (.y out) (/ (.height out) 2))))))
                 (if (< (.x self) (.x out))
                     (if (< (.y self) (.y out))
                         (progn
                           (setf x1 (+ (.x self) (/ (.width self) 2)))
                           (setf y1 (+ (.y self) (.height self)))
                           (setf x2 (+ (.x out) (/ (.width out) 2)))
                           (setf y2 (.y out)))
                         (progn
                           (setf x1 (+ (.x self) (/ (.width self) 2)))
                           (setf y1 (.y self))
                           (setf x2 (+ (.x out) (/ (.width out) 2)))
                           (setf y2 (+ (.y out) (.height out)))))
                     (if (< (.y self) (.y out))
                         (progn
                           (setf x1 (+ (.x self) (/ (.width self) 2)))
                           (setf y1 (+ (.y self) (.height self)))
                           (setf x2 (+ (.x out) (/ (.width out) 2)))
                           (setf y2 (.y out)))
                         (progn
                           (setf x1 (+ (.x self) (/ (.width self) 2)))
                           (setf y1 (.y self))
                           (setf x2 (+ (.x out) (/ (.width out) 2)))
                           (setf y2 (+ (.y out) (.height out)))))))
             (sdl2:render-draw-line renderer x1 y1 x2 y2)
             (sdl2:render-draw-rect renderer (sdl2:make-rect (- x1 2) (- y1 2) 5 5))))
  (call-next-method))

(defclass module (renderable)
  ((name :initarg :name :initform "noname" :accessor .name)))

(defmethod initialize-instance :after ((self module) &key)
  (add-child self (make-instance 'text :value (.name self) :x 3 :y 3)))

(defmethod click ((self renderable) x y))

(defmethod keydown ((self renderable) scancode mod-value))

(defmethod keyup ((self renderable) scancode mod-value))

(defclass text (renderable)
  ((value :initarg :value :initform "Hi" :accessor .value)))

(defmethod render ((self text) renderer)
  ;; TODO 毎回 surface, texture 作るのは無駄だね？
  (let* ((surface (apply #'sdl2-ttf:render-utf8-solid (.font *app*) (.value self) (.color self)))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (sdl2:render-copy renderer
                      texture
                      :source-rect nil
                      :dest-rect (sdl2:make-rect (.x self) (.y self) width height))
    (sdl2:destroy-texture texture)))

(defclass tracker (renderable)
  ((pattern :accessor .pattern)
   (lines :initform nil :accessor .lines)
   (cursor-x :initform 0 :accessor .cursor-x)
   (cursor-y :initform 0 :accessor .cursor-y)
   ))

(defmethod render :before ((self tracker) renderer)
  (let ((pattern-lines (.lines (.pattern self))))
    (if (/= (length pattern-lines)
            (length (.lines self)))
        (progn
          (loop for line in (.lines self)
                do (remove-child self line))
          (setf (.lines self) nil)
          (loop for pattern-line in pattern-lines
                for y from 2 by 10
                for line = (make-instance 'tracker-line :line pattern-line
                                                        :x 3 :y y)
                do (push line (.lines self))
                   (add-child self line)))
        (loop for pattern-line in pattern-lines
              for tracker-line in (.lines self)
              do (setf (.line tracker-line) pattern-line)))))

(defparameter *char-width* 5)
(defparameter *char-height* 10)
(defparameter *cursor-color* '(#x00 #x00 #xcc #x80))

(defmethod render ((self tracker) renderer)
  (when (eq (.parent self) (module-at-mouse *app*))
    (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
    (sdl2:render-fill-rect
     renderer
     (sdl2:make-rect (+ (* (.cursor-x self) *char-width*)  (.x self) 2)
                     (+ (* (.cursor-y self) *char-height*) (.y self) 2)
                     (if (zerop (.cursor-x self)) (* 2  *char-width*) *char-width*)
                     *char-height*)))
  (call-next-method))

(defmethod keydown ((self tracker) scancode mod-value)
  (cond ((or (sdl2:scancode= scancode :scancode-up)
             (sdl2:scancode= scancode :scancode-k))
         (setf (.cursor-y self)
               (max (1- (.cursor-y self)) 0)))
        ((or (sdl2:scancode= scancode :scancode-down)
             (sdl2:scancode= scancode :scancode-j))
         (print (setf (.cursor-y self)
                (min (1+ (.cursor-y self))
                     (1- (length (.lines (.pattern self))))))))
        ((and (or (sdl2:scancode= scancode :scancode-left)
                  (sdl2:scancode= scancode :scancode-h))
              (= (.cursor-x self) 2))
         (setf (.cursor-x self) 0))
        ((and (or (sdl2:scancode= scancode :scancode-right)
                  (sdl2:scancode= scancode :scancode-l))
              (= (.cursor-x self) 0))
         (setf (.cursor-x self) 2))))

(defclass tracker-line (text)
  ((line :initarg :line :accessor .line)))

(defmethod render :before ((self tracker-line) renderer)
  (let ((string (format nil "~a" (midino-to-note (.line self)))))
    (when (char/= (char string 1) #\#)
      (setf string (format nil "~a-~a" (char string 0) (char string 1))))
    (setf (.value self) string)))

(defclass sequencer-module (sequencer module)
  ()
  (:default-initargs :name "sequencer" :color (list #x00 #xff #xff *transparency*)))

(defmethod click ((self sequencer-module) x y)
  (declare (ignore x y))
  (if (.playing *audio*)
      (stop)
      (play)))

(defclass pattern-module (pattern module)
  ((tracker :initform (make-instance 'tracker) :accessor .tracker))
  (:default-initargs :name "pattern"))

(defmethod keydown ((self pattern-module) scancode mod-value)
  (keydown (.tracker self) scancode mod-value))

(defmethod initialize-instance :after ((self pattern-module) &key)
  (let ((tracker (.tracker self)))
    (add-child self tracker)
    (setf (.pattern tracker) self
          (.x tracker) 5
          (.y tracker) 15
          (.width tracker) (- (.width self) 10)
          (.height tracker) (- (.height self) 20))))

(defclass osc-module-mixin ()
  ((value-text :initform (make-instance 'text :value "0" :x 20 :y 20)
               :accessor .value-text)))

(defmethod initialize-instance :after ((self osc-module-mixin) &key)
  (add-child self (.value-text self)))

(defmethod render :before ((self osc-module-mixin) renderer)
  ;; TODO 依存性の何とかとか
  (setf (.value (.value-text self)) (format nil "~,5f" (.value self))))

(defclass sin-osc-module (sin-osc module osc-module-mixin)
  ()
  (:default-initargs :name "sin"))

(defclass saw-osc-module (saw-osc module osc-module-mixin)
  ()
  (:default-initargs :name "saw"))

(defclass adsr-module (adsr module)
  ()
  (:default-initargs :name "adsr"))

(defclass amp-module (amp module)
  ()
  (:default-initargs :name "amp"))

(defclass master-module (master module)
  ()
  (:default-initargs :name "master" :color (list #xff #xa5 #x00 *transparency*)))


(defun main ()
  (sb-thread:make-thread 'main-loop))

(defun main-loop ()
  (let ((*app* (setf *app* (make-instance
                            'app
                            :width 800
                            :height 600))))
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
            (let* ((sequencer (.sequencer *audio*))
                   (master (.master *audio*))
                   (pattern1 (make-instance 'pattern-module
                                            :lines (list a4 e4 none g4
                                                         a4 off  g4 c4)
                                            :x 125 :y 5))
                   (osc1 (make-instance 'sin-osc-module :x 245 :y 5))
                   (adsr1 (make-instance 'adsr-module :d 0.2d0 :s 0d0
                                                      :x 365 :y 5))
                   (amp1 (make-instance 'amp-module
                                        :x 485 :y 5))
                   (pattern2 (make-instance 'pattern-module
                                            :lines (list a3 e3 none g3
                                                         a3 off  g3 c3)
                                            :x 125 :y 100))
                   (osc2 (make-instance 'saw-osc-module
                                        :x 245 :y 100))
                   (adsr2 (make-instance 'adsr-module :d 0.7d0 :s 0.8d0
                                                      :x 365 :y 100))
                   (amp2 (make-instance 'amp-module
                                        :x 485 :y 100)))
              (connect pattern1 osc1)
              (connect pattern1 adsr1)
              (connect osc1 amp1)
              (connect adsr1 amp1)
              (connect amp1 master)
              (connect pattern2 osc2)
              (connect pattern2 adsr2)
              (connect osc2 amp2)
              (connect adsr2 amp2)
              (connect amp2 master)
              (add-pattern sequencer pattern1 0)
              (add-pattern sequencer pattern1 (length (.lines pattern1)))
              (add-pattern sequencer pattern2 (length (.lines pattern1)))
              (add-pattern sequencer pattern1 (* 2 (length (.lines pattern1))))
              (add-pattern sequencer pattern2 (* 2 (length (.lines pattern1))))
              (setf (.modules *app*)
                    (list sequencer master
                          pattern1 osc1 adsr1 amp1 
                          pattern2 osc2 adsr2 amp2))
              (sdl2:with-event-loop (:method :poll)
                (:keydown (:keysym keysym)
                          (handle-sdl2-keydown-event keysym))
                (:keyup (:keysym keysym)
                        (handle-sdl2-keyup-event keysym))
                (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                              (handle-sdl2-mousemotion-event x y xrel yrel state))
                (:mousebuttondown (:button button :state state :clicks clicks :x x :y y)
                                  (handle-sdl2-mousebuttondown-event button state clicks x y))
                (:mousebuttonup (:button button :state state :clicks clicks :x x :y y)
                                (handle-sdl2-mousebuttonup-event button state clicks x y))
                (:idle ()
                       (handle-sdl2-idle-event renderer))
                (:quit ()
                       (handle-sdl2-quit-event)
                       t)))))))))

(defun handle-sdl2-keydown-event (keysym)
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
            mod-value)
    (awhen (module-at-mouse *app*)
      (keydown it scancode mod-value))))

(defun handle-sdl2-keyup-event (keysym)
  #+nil
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit))
  (let  ((scancode (sdl2:scancode-value keysym))
         (mod-value (sdl2:mod-value keysym)))
    (awhen (module-at-mouse *app*)
      (keyup it scancode mod-value))))

(defun handle-sdl2-mousemotion-event (x y xrel yrel state)
  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
          x xrel y yrel state)
  (setf (.mouse-x *app*) x)
  (setf (.mouse-y *app*) y)
  (awhen (.drag-module *app*)
    (incf (.x it) xrel)
    (incf (.y it) yrel)))


(defun handle-sdl2-mousebuttondown-event (button state clicks x y)
  (format t "Mouse button down button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (case button
    (1                                  ;left
     (let ((module (module-at-mouse *app*)))
       (setf (.mouse-left-down *app*) t
             (.drag-module *app*) module)
       (when module
         (click module (- x (.x module)) (- y (.y module))))))
    (3                                  ;right
     (setf (.connect-from-module *app*) (module-at-mouse *app*)))))

(defun handle-sdl2-mousebuttonup-event (button state clicks x y)
  (format t "Mouse button up button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (case button
    (1                                  ;left
     (setf (.mouse-left-down *app*) nil
           (.drag-module *app*) nil))
    (3                                  ;right
     (let ((from (.connect-from-module *app*)))
       (when from
         (let ((to (module-at-mouse *app*)))
           (if (and to (not (eq from to)))
               (if (member to (.out from))
                   (disconnect from to)
                   (connect from to)))))
       (setf (.connect-from-module *app*) nil)))))

(defun handle-sdl2-idle-event (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 #xff)
  (sdl2:render-clear renderer)
  (sdl2:set-render-draw-color renderer #xcc #xcc #xcc *transparency*)

  (loop for module in (.modules *app*)
        do (render module renderer))
  
  (sdl2:render-present renderer)
  (when (.request-stop *audio*)
    (stop))
  (sdl2:delay #.(floor (/ 1000 60.0))))   ;ms

(defun handle-sdl2-quit-event ()
  (when (.font *app*)
    (sdl2-ttf:close-font (.font *app*))
    (setf (.font *app*) nil))
  (when (= 1 (sdl2-ttf:was-init))
    (print 'sdl2-ttf:quit)
    (sdl2-ttf:quit)))
