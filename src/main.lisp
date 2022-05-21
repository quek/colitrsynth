(in-package :colitrsynth)

(defparameter *font-size* 14)
(defparameter *char-width* (/ *font-size* 2))
(defparameter *char-height* *font-size*)
(defparameter *cursor-color* '(#x00 #x00 #xcc #x80))
(defparameter *play-position-color* '(#x00 #x80 #x00 #x80))
(defparameter *connection-line-color* '(#x22 #x8b #x22 #x80))
(defparameter *connection-point-color* '(#xff #xff #xff #x80))
(defparameter *selected-module-color* '(#xff #xff #x00 #xff))
(defparameter *selected-pattern-color* '(#xff #x80 #x80 #xff))
(defparameter *pixcel-per-line* 5)
(defparameter *layout-space* 5)
(defparameter *plugin-host-exe* "C:/Users/ancient/Documents/Visual Studio 2022/PluginHost/Builds/VisualStudio2022/x64/Debug/App/PluginHost.exe")
(defparameter *plugin-host-pipe-name* "\\\\.\\pipe\\pluin-host")

(defparameter *transparency* #xc0)

(defconstant +mouse-button-count+ 16)

(defvar *app*)

(defgeneric render (self renderer)
  (:method (self renderer)))

(defgeneric mousebuttondown (self button state clicks x y)
  (:method (self button state clicks x y)
    (setf (click-target-module button) self)))

(defgeneric mousebuttonup (self button state clicks x y)
  (:method (self button state clicks x y)
    (when (eq self (click-target-module button))
      (let ((root (.root-parent self)))
        (click root button
               (- (.mouse-x *app*) (.absolute-x root))
               (- (.mouse-y *app*) (.absolute-y root)))))))

(defgeneric click (self button x y)
  (:method (self button x y)))

(defgeneric drag-start (self x y button)
  (:method (self x y button)))

(defgeneric drag (self xrel yrel button)
  (:method (self xrel yrel button)))

(defgeneric drag-end (self x y button)
  (:method (self x y button)))

(defgeneric drop (self dropped x y button)
  (:method (self dropped x y button)))

(defgeneric mousemotion (self x y xrel yrel state)
  (:method (self x y xrel yrel state)))

(defgeneric keydown (self value scancode mod-value)
  (:method (self value scancode mod-value)))

(defgeneric keyup (self value scancode mod-value)
  (:method (self value scancode mod-value)))

(defgeneric move (self xrel yrel)
  (:method (self xrel yrel)))

(defgeneric resize (self xrel yrel)
  (:method (self xrel yrel)))

(defgeneric .target (self)
  (:method ((self null)) nil))

(defclass app ()
  ((win :initarg :win :accessor .win)
   (width :initarg :width :initform 800 :accessor .width)
   (height :initarg :height :initform 600 :accessor .height)
   (font :initform nil :accessor .font)
   (modules :initarg :modules :initform '() :accessor .modules)
   (mouse-x :initform 0 :accessor .mouse-x)
   (mouse-y :initform 0 :accessor .mouse-y)
   (selected-module :initform nil :accessor .selected-module)
   (selected-pattern :initform nil :accessor .selected-pattern)
   (click-target-module :initform (make-array +mouse-button-count+))
   (drag-resize-module :initform nil :accessor .drag-resize-module)
   (dragging :initform nil :accessor .dragging)
   (drag-state :initform nil :accessor .drag-state)
   (connect-from-module :initform nil :accessor .connect-from-module)))

(defclass drag-state ()
  ((target :initarg :target :accessor .target)
   (button :initarg :button :accessor .button)
   (x :initarg :x :accessor .x)
   (y :initarg :y :accessor .y)
   (state :initarg :state :accessor .state)
   (dragging :initform nil :accessor .dragging)))

(defclass function-value-mixin ()
  ((value :initarg :value :initform 0.0d0)))

(defmethod .value ((self function-value-mixin))
  (let ((value (slot-value self 'value)))
    (if (functionp value)
        (funcall value)
        value)))

(defmethod (setf .value) (value (self function-value-mixin))
  (setf (slot-value self 'value) value))


(defun click-target-module (button)
  (aref (slot-value *app* 'click-target-module) button))

(defun (setf click-target-module) (value button)
  (setf (aref (slot-value *app* 'click-target-module) button) value))

(defun add-module (module)
  (push module (.modules *app*)))

(defun remove-module (module)
  (setf (.modules *app*) (remove module (.modules *app*))))

(defun module-at-mouse (app)
  (loop for module in (.modules app)
          thereis (and (<= (.absolute-x module) (.mouse-x app) (+ (.absolute-x module) (.width module)))
                       (<= (.absolute-y module) (.mouse-y app) (+ (.absolute-y module) (.height module)))
                       module)))

(defun child-module-at (self x y)
  (loop for module in (.children self)
          thereis (and (<= (.x module) x (+ (.x module) (.width module)))
                       (<= (.y module) y (+ (.y module) (.height module)))
                       module)))

(defclass renderable ()
  ((color :initarg :color :initform (list #xdd #xdd #xdd *transparency*) :accessor .color)
   (x :initarg :x :initform 0 :accessor .x)
   (y :initarg :y :initform 0 :accessor .y)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)
   (parent :initarg :parent :initform nil :accessor .parent)
   (children :initarg :children :initform nil :accessor .children)))

(defmethod .root-parent ((self renderable))
  (aif (.parent self)
       (.root-parent it)
       self))

(defmethod mousebuttondown ((self renderable) button state clicks x y)
  (call-next-method)
  (awhen (child-module-at self x y)
    (mousebuttondown it button state clicks
                     (- x (.x it)) (- y (.y it)))))

(defmethod mousebuttonup ((self renderable) button state clicks x y)
  (call-next-method)
  (awhen (child-module-at self x y)
    (mousebuttonup it button state clicks
                   (- x (.x it)) (- y (.y it)))))

(defmethod click ((self renderable) button x y)
  (call-next-method)
  (awhen (child-module-at self x y)
    (click it button (- x (.x it)) (- y (.y it)))))

(defmethod drop ((self renderable) dropped x y button)
  (call-next-method)
  (awhen (child-module-at self x y)
    (drop it dropped (- x (.x it)) (- y (.y it)) button)))

(defmethod mousemotion ((self renderable) x y xrel yrel state)
  (call-next-method)
  (awhen (child-module-at self x y)
   (mousemotion it (- x (.x it)) (- y (.y it))
                xrel yrel state)))

(defmethod move ((self renderable) xrel yrel)
    (incf (.x self) xrel)
    (incf (.y self) yrel))

(defmethod resize ((self renderable) xrel yrel)
  (setf (.width self) (max 20 (+ (.width self) xrel)))
  (setf (.height self) (max 20 (+ (.height self) yrel))))

(defmethod add-child ((parent renderable) (child renderable))
  (unless (eq parent (.parent child))
    (setf (.children parent) (append (.children parent) (list child)))
    (setf (.parent child) parent)))

(defmethod remove-child ((parent renderable) (child renderable))
  (setf (.children parent) (remove child (.children parent)))
  (setf (.parent child) nil))

(defmethod .x ((self null))
  0)

(defmethod .absolute-x ((self null))
  0)

(defmethod .absolute-x ((self renderable))
  (+ (.x self)
     (.absolute-x (.parent self))))

(defmethod .absolute-center-x ((self renderable))
  (+ (.absolute-x self)
     (round (/ (.width self) 2))))

(defmethod .y ((self null))
  0)

(defmethod .absolute-y ((self null))
  0)

(defmethod .absolute-center-y ((self renderable))
  (+ (.absolute-y self)
     (round (/ (.height self) 2))))

(defmethod .absolute-y ((self renderable))
  (+ (.y self)
     (.absolute-y (.parent self))))

(defmethod render ((self renderable) renderer)
  (with-slots (color width height) self
    (apply #'sdl2:set-render-draw-color renderer color)
    (sdl2:render-draw-rect renderer (sdl2:make-rect (.absolute-x self) (.absolute-y self) width height)))
  (loop for child in (.children self)
        do (render child renderer))
  (call-next-method))

(defmethod render ((self audio-module) renderer)
  (flet ((intersec (ax ay bx by cx cy dx dy)
           (let* ((deno (- (* (- bx ax) (- dy cy))
                           (* (- by ay) (- dx cx)))))
             (if (= deno 0)
                 nil
                 (let* ((c-a-x (- cx ax))
                        (c-a-y (- cy ay))
                        (d-c-x (- dx cx))
                        (d-c-y (- dy cy))
                        (p (/ (- (* c-a-x d-c-y)
                                 (* c-a-y d-c-x))
                              deno))
                        (b-a-x (- bx ax))
                        (b-a-y (- by ay))
                        (a-c-x (- ax cx))
                        (a-c-y (- ay cy))
                        (q (/ (- (* b-a-x a-c-y)
                                 (* b-a-y a-c-x))
                              deno)))
                   (if (or (< p 0) (< 1 p) (< q 0) (< 1 q))
                       nil
                       (cons (round (+ ax (* p (- bx ax))))
                             (round (+ ay (* p (- by ay)))))))))))
    (loop for out in (.out self)
          do (let* ((x1 (.absolute-center-x self))
                    (y1 (.absolute-center-y self))
                    (x2 (.absolute-center-x out))
                    (y2 (.absolute-center-y out))
                    (xs1 (.absolute-x self))
                    (ys1 (.absolute-y self))
                    (xs2 (+ xs1 (.width self)))
                    (ys2 ys1)
                    (xs3 xs2)
                    (ys3 (+ ys2 (.height self)))
                    (xs4 xs1)
                    (ys4 ys3)
                    (xe1 (.absolute-x out))
                    (ye1 (.absolute-y out))
                    (xe2 (+ xe1 (.width out)))
                    (ye2 ye1)
                    (xe3 xe2)
                    (ye3 (+ ye2 (.height out)))
                    (xe4 xe1)
                    (ye4 ye3))
               (destructuring-bind (xs . ys)
                   (or (intersec x1 y1 x2 y2 xs1 ys1 xs2 ys2)
                       (intersec x1 y1 x2 y2 xs2 ys2 xs3 ys3)
                       (intersec x1 y1 x2 y2 xs3 ys3 xs4 ys4)
                       (intersec x1 y1 x2 y2 xs4 ys4 xs1 ys1)
                       (cons x1 y1))
                 (destructuring-bind (xe . ye)
                     (or (intersec x1 y1 x2 y2 xe1 ye1 xe2 ye2)
                         (intersec x1 y1 x2 y2 xe2 ye2 xe3 ye3)
                         (intersec x1 y1 x2 y2 xe3 ye3 xe4 ye4)
                         (intersec x1 y1 x2 y2 xe4 ye4 xe1 ye1)
                         (cons x2 y2))
                   (apply #'sdl2:set-render-draw-color renderer *connection-line-color*)
                   (sdl2:render-draw-line renderer xs ys xe ye)
                   (apply #'sdl2:set-render-draw-color renderer *connection-point-color*)
                   (sdl2:render-fill-rect renderer
                                          (sdl2:make-rect (- xs 2) (- ys 2) 5 5)))))))
  (call-next-method))

(defclass name-mixin ()
  ((name :initarg :name :initform "noname" :accessor .name)))

(defmethod initialize-instance :after ((self name-mixin) &key)
  (let ((text (make-instance 'text :value (.name self) :x 3 :y 3)))
    (add-child self text)
    (defmethod (setf .name) (value (self (eql self)))
      (setf (.value text) value))))

(defclass module (name-mixin
                  drag-resize-mixin
                  drag-move-mixin
                  renderable
                  drag-connect-mixin)
  ())

(defmethod close ((self module) &key abort)
  (declare (ignore abort)))

(defmethod keydown ((self module) value scancode mod-value)
  (if (sdl2:scancode= scancode :scancode-delete)
      (progn
       (disconnect-all self)
       (remove-module self)
       (close self))
      (call-next-method)))

(defmethod mousebuttondown :before ((self null) button state clicks x y)
  (setf (.selected-module *app*) nil))

(defmethod mousebuttondown :before ((self module) button state clicks x y)
  (setf (.selected-module *app*) self)
  (setf (.modules *app*)
        (stable-sort (.modules *app*) (lambda (x y)
                                        (declare (ignore y))
                                        (eql x self)))))

(defmethod render :after ((self module) renderer)
  (let ((color (cond ((eq self (.selected-pattern *app*))
                      *selected-pattern-color*)
                     ((eq self (.selected-module *app*))
                      *selected-module-color*))))
    (when color
      (apply #'sdl2:set-render-draw-color renderer color)
      (sdl2:render-draw-rect
       renderer
       (sdl2:make-rect (- (.absolute-x self) 2)
                       (- (.absolute-y self) 2)
                       (+ (.width self) 4)
                       (+ (.height self) 4))) )))

(defclass drag-mixin ()
  ())

(defmethod mousebuttondown ((self drag-mixin) button state clicks x y)
  (setf (.drag-state *app*)
        (make-instance 'drag-state :target self :button button
                                   :x x :y y :state state))
  (call-next-method))

(defmethod mousemotion ((self drag-mixin) x y xrel yrel state)
  (let ((drag-state (.drag-state *app*)))
    (if (eq self (.target drag-state))
        (progn
          (sunless (.dragging drag-state)
            (setf it t)
            (drag-start self x y (.button drag-state)))
          (drag self xrel yrel (.button drag-state)))
        (call-next-method))))

(defmethod mousebuttonup ((self drag-mixin) button state clicks x y)
  (let ((drag-state (.drag-state *app*)))
    (if (eq self (.target drag-state))
        (progn
          (if (.dragging drag-state)
              (progn
                (drag-end self x y button)
                (drop (module-at-mouse *app*) self
                      (+ (.absolute-x self) x)
                      (+ (.absolute-y self) y)
                      button))
              (call-next-method))
          (setf drag-state nil))
        (call-next-method)))
  (setf (.drag-state *app*) nil))

(defclass drop-mixin ()
  ())

(defclass drag-move-mixin (drag-mixin)
  ())

(defmethod drag ((self drag-move-mixin) xrel yrel (button (eql 1)))
  (move self xrel yrel))

(defclass drag-resize-mixin (drag-mixin)
  ())

(defmethod drag-start ((self drag-resize-mixin) x y (button (eql 1)))
  (if (and (< (.width self) (+ 10 x))
           (< (.height self) (+ 10 y)))
      (setf (.drag-resize-module *app*) self)
      (call-next-method)))

(defmethod drag ((self drag-resize-mixin) xrel yrel (button (eql 1)))
  (if (eq self (.drag-resize-module *app*))
      (resize self xrel yrel)
      (call-next-method)))

(defmethod drag-end ((self drag-resize-mixin) x y (button (eql 1)))
  (when (eq self (.drag-resize-module *app*))
    (setf (.drag-resize-module *app*) nil))
  (call-next-method))

(defmethod render ((self drag-resize-mixin) renderer)
  (call-next-method)
  (apply #'sdl2:set-render-draw-color renderer (.color self))
  (sdl2:render-draw-line renderer
                         (+ (.absolute-x self) (.width self) -10)
                         (+ (.absolute-y self) (.height self) -1)
                         (+ (.absolute-x self) (.width self) -1)
                         (+ (.absolute-y self) (.height self) -10))
  (sdl2:render-draw-line renderer
                         (+ (.absolute-x self) (.width self) -7)
                         (+ (.absolute-y self) (.height self) -1)
                         (+ (.absolute-x self) (.width self) -1)
                         (+ (.absolute-y self) (.height self) -7)))

(defclass drag-connect-mixin ()
  ((connecting :initform nil :accessor .connecting)))

(defmethod render :after ((self drag-connect-mixin) renderer)
  (when (eq self (.connect-from-module *app*))
    (apply #'sdl2:set-render-draw-color renderer *connection-line-color*)
    (sdl2:render-draw-line renderer
                           (.absolute-center-x self)
                           (.absolute-center-y self)
                           (.mouse-x *app*)
                           (.mouse-y *app*))))

(defmethod drag-start ((self drag-connect-mixin) x y (button (eql 3)))
  (setf (.connect-from-module *app*) self)
  (call-next-method))

(defmethod drop ((self drag-connect-mixin) (dropped drag-connect-mixin) x y (button (eql 3)))
  (let ((from (.connect-from-module *app*)))
    (when (and from (not (eq from self)))
      (if (member self (.out from))
          (disconnect from self)
          (connect from self)))
    (setf (.connect-from-module *app*) nil))
  (call-next-method))

(defmethod drop ((self sequencer-module-track) (dropped drag-connect-mixin) x y (button (eql 3))))

(defclass disable-drag-connect-mixin ()
  ())

(defmethod drag-start ((self disable-drag-connect-mixin) x y (button (eql 3))))

(defmethod drop ((self disable-drag-connect-mixin) dropped x y (button (eql 3))))

(defclass text (function-value-mixin renderable)
  ((lat-value :initform "" :accessor .last-value)
   (texture :initform nil :accessor .texture))
  (:default-initargs :width 0 :height 0 :value "くえっ"))

(defmethod render ((self text) renderer)
  (let ((value (.value self)))
    (when (string/= value "")
      (when (string/= value (.last-value self))
        (setf (.last-value self) value)
        (awhen (.texture self)
          (sdl2:destroy-texture it))
        (let ((surface (apply #'sdl2-ttf:render-utf8-solid (.font *app*)
                              value
                              (.color self))))
          (setf (.width self) (sdl2:surface-width surface)
                (.height self) (sdl2:surface-height surface)
                (.texture self) (sdl2:create-texture-from-surface renderer surface))))
      (sdl2:render-copy renderer
                        (.texture self)
                        :source-rect nil
                        :dest-rect (sdl2:make-rect (.absolute-x self) (.absolute-y self)
                                                   (.width self) (.height self))))))

(defclass button (renderable)
  ()
  (:default-initargs :width 50 :height 30))

(defmethod initialize-instance :after ((self button) &key text)
  (add-child self (make-instance 'text :value text :x 5 :y 2)))

(defmethod .label ((self button))
  (.value (car (.children self))))

(defmethod render :after ((self button) renderer)
  ;; after でやるので初回描画時は崩れてるはずだけど妥協
  (let ((text (car (.children self))))
    (setf (.width self) (+ 10 (.width text))
          (.height self) (+ 4 (.height text)))))

(defclass onchange-mixin ()
  ((onchange :initarg :onchange :initform (constantly nil) :accessor .onchange)))

(defclass slider (onchange-mixin
                  function-value-mixin drag-mixin renderable)
  ((min :initarg :min :initform 0.0d0 :accessor .min)
   (max :initarg :max :initform 1.0d0 :accessor .max)))

(defmethod initialize-instance :after ((self slider) &key)
  (add-child self (make-instance 'text :value (lambda () (format nil "~,5f" (.value self)))
                                       :x *layout-space* :y (round (/ *layout-space*)))))

(defmethod render ((self slider) renderer)
  (call-next-method)
  (let ((x (+ (round (* (/ (- (.value self) (.min self))
                           (.max self))
                        (.width self)))
              (.absolute-x self))))
    (sdl2:set-render-draw-color renderer #xff #x00 #xff #xff)
    (sdl2:render-draw-line renderer
                           x (1+ (.absolute-y self))
                           x (+ (.absolute-y self) (.height self) -2))))

(defmethod drag ((self slider) xrel yrel button)
  (funcall (.onchange self) (min (.max self)
                                 (max (.min self)
                                      (+ (.value self) (/ xrel 100.0))))))

(defclass pattern-editor (renderable)
  ((pattern :accessor .pattern)
   (lines :initform nil :accessor .lines)
   (cursor-x :initform 0 :accessor .cursor-x)
   (cursor-y :initform 0 :accessor .cursor-y)
   (octave :initform 4 :accessor .octave)
   (edit-step :initform 0 :accessor .edit-step)))

(defmethod render :before ((self pattern-editor) renderer)
  (let ((pattern-lines (.lines (.pattern self))))
    (if (/= (length pattern-lines)
            (length (.lines self)))
        (progn
          (loop for line in (.lines self)
                do (remove-child self line))
          (setf (.lines self) nil)
          (loop for pattern-line across pattern-lines
                for y from 2 by *char-height*
                for line = (make-instance 'pattern-editor-line :line pattern-line
                                                        :x 3 :y y)
                do (push line (.lines self))
                   (add-child self line))
          (setf (.lines self) (nreverse (.lines self))))
        (loop for pattern-line across pattern-lines
              for pattern-editor-line in (.lines self)
              do (setf (.line pattern-editor-line) pattern-line)))))

(defmethod render ((self pattern-editor) renderer)
  (let ((texture (multiple-value-call
                     #'sdl2:create-texture renderer :rgba8888 :target
                   (multiple-value-call
                       ;; window からはみ出ると render-copy でひずむので
                       (lambda (w h) (values (+ w (.width self))
                                             (+ h (.height self))))
                     (sdl2:get-window-size (.win *app*))))))
    (unwind-protect
         (let ((play-x (+ (* (.cursor-x self) *char-width*)  (.absolute-x self) 2))
               (play-y (+ (* (.current-line (.pattern self)) *char-height*) (.absolute-y self) 2))
               (play-w (.width self))
               (play-h *char-height*)
               (cursor-x (+ (* (.cursor-x self) *char-width*)  (.absolute-x self) 2))
               (cursor-y (+ (* (.cursor-y self) *char-height*) (.absolute-y self) 2))
               (cursor-w (if (zerop (.cursor-x self)) (* 3 *char-width*) *char-width*))
               (cursor-h *char-height*))
           (sdl2:set-render-target renderer texture)
           (sdl2:set-texture-blend-mode texture :blend)
           (sdl2:set-render-draw-color renderer 0 0 0 #x00)
           (sdl2:render-clear renderer)
           ;; play position
           (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
           (sdl2:render-fill-rect
            renderer
            (sdl2:make-rect play-x play-y play-w play-h))
           ;; cursor position
           (when (eq (.parent self) (.selected-module *app*))
             (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
             (sdl2:render-fill-rect
              renderer
              (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h)))
           (loop for child in (.children self)
                 do (render child renderer))
           (sdl2:set-render-target renderer nil)
           (let* ((dst-x (.absolute-x self))
                  (dst-y (.absolute-y self))
                  (dst-w (.width self))
                  (dst-h (.height self))
                  (offset-y (- play-y dst-y (round (/ dst-h 2)) (- (round (/ *char-height* 2)))))
                  (src-x dst-x)
                  (src-y (+ dst-y offset-y))
                  (src-w dst-w)
                  (src-h dst-h)
                  (dst-rect (sdl2:make-rect dst-x dst-y dst-w dst-h))
                  (src-rect (sdl2:make-rect src-x src-y src-w src-h)))
             (sdl2:render-copy renderer texture :source-rect src-rect :dest-rect dst-rect)))
      (sdl2:destroy-texture texture))))

(defmethod keydown ((self pattern-editor) value scancode mod-value)
  (flet ((set-note (note)
           (setf (.note (aref (.lines (.pattern self)) (.cursor-y self)))
                 note)))
    (cond ((or (sdl2:scancode= scancode :scancode-up)
               (sdl2:scancode= scancode :scancode-k))
           (setf (.cursor-y self)
                 (max (1- (.cursor-y self)) 0)))
          ((or (sdl2:scancode= scancode :scancode-down)
               (sdl2:scancode= scancode :scancode-j))
           (setf (.cursor-y self)
                 (min (1+ (.cursor-y self))
                      (1- (length (.lines (.pattern self)))))))
          #+nil
          ((and (or (sdl2:scancode= scancode :scancode-left)
                    (sdl2:scancode= scancode :scancode-h))
                (= (.cursor-x self) 2))
           (setf (.cursor-x self) 0))
          #+nil
          ((and (or (sdl2:scancode= scancode :scancode-right)
                    (sdl2:scancode= scancode :scancode-l))
                (= (.cursor-x self) 0))
           (setf (.cursor-x self) 2))
          ((sdl2:scancode= scancode :scancode-q)
           (set-note (+ c0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-2)
           (set-note (+ c#0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-w)
           (set-note (+ d0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-3)
           (set-note (+ d#0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-e)
           (set-note (+ e0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-r)
           (set-note (+ f0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-5)
           (set-note (+ f#0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-t)
           (set-note (+ g0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-6)
           (set-note (+ g#0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-y)
           (set-note (+ a0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-7)
           (set-note (+ a#0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-u)
           (set-note (+ b0 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-i)
           (set-note (+ c1 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-9)
           (set-note (+ c#1 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-o)
           (set-note (+ d1 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-0)
           (set-note (+ d#1 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-p)
           (set-note (+ e1 (* 12 (.octave self)))))
          ((= scancode 47)               ;@
           (set-note (+ f1 (* 12 (.octave self)))))
          ((= scancode 46)               ;^
           (set-note (+ f#1 (* 12 (.octave self)))))
          ((= scancode 48)               ;[
           (set-note (+ g1 (* 12 (.octave self)))))
          ((= scancode 137)              ;\
           (set-note (+ g#1 (* 12 (.octave self)))))
          ((sdl2:scancode= scancode :scancode-a)
           (set-note off))
          ((sdl2:scancode= scancode :scancode-delete)
           (set-note none)))))
;;(sdl2:scancode-key-to-value :scancode-delete)

(defclass pattern-editor-line (text)
  ((line :initarg :line :accessor .line)))

(defmethod render :before ((self pattern-editor-line) renderer)
  (let* ((midino (.note (.line self)))
         (string (case midino
                   (#.off "OFF")
                   (#.none "---")
                   (t
                    (let ((string (format nil "~a" (midino-to-note midino))))
                      (if (char/= (char string 1) #\#)
                          (format nil "~a-~a" (char string 0) (char string 1))
                          string))))))
    (setf (.value self) string)))

(defparameter *track-height* 40)        ;TODO 固定長で妥協

(defclass sequencer-module-track (track
                                  drag-mixin
                                  drag-connect-mixin
                                  drop-mixin
                                  renderable)
  ()
  (:default-initargs :width 690 :height *track-height*))

(defun pixcel-to-line (pixcel)
  (* (round (/ pixcel *pixcel-per-line*) 4) 4))

(defun line-to-pixcel (line)
  (* *pixcel-per-line* line))

(defmethod click ((self sequencer-module-track) button x y)
  (case button
    (1
     (let ((module (.selected-pattern *app*)))
       (if (typep module 'pattern-module)
           (let* ((start (pixcel-to-line x))
                  (end (+ start (.length module))))
             (when (every (lambda (x)
                            (or (<= end (.start x))
                                (<= (.end x) start)))
                          (.pattern-positions self))
               (add-pattern self module start end)))
           (call-next-method))))
    (3
     (awhen (child-module-at self x y)
       (remove-pattern self it))
     (call-next-method))))

(defclass pattern-position (pattern-position-mixin
                            drag-mixin
                            renderable
                            name-mixin)
  ((move-delta-x :initform 0 :accessor .move-delta-x)))

(defmethod drag ((self pattern-position) xrel yrel button)
  (let* ((pixcel (+ (.x self) (.move-delta-x self) xrel))
         (line (pixcel-to-line pixcel))
         (rounded-pixcel (line-to-pixcel line)))
    (setf (.x self) rounded-pixcel
          (.move-delta-x self) (- pixcel rounded-pixcel))))

(defmethod drag-end ((self pattern-position) x y button)
  (setf (.move-delta-x self) 0)
  (call-next-method))

(defmethod drop ((self sequencer-module-track) (pattern-position pattern-position) x y button)
  (let ((delta (- (pixcel-to-line (.x pattern-position)) (.start pattern-position))))
    (incf (.start pattern-position) delta)
    (incf (.end pattern-position) delta)
    (update-sequencer-end)))

(defclass sequencer-module (sequencer
                            module)
  ()
  (:default-initargs :name ""
                     :color (list #x00 #xff #xff *transparency*)
                     :width 700
                     :height 200))

(defmethod initialize-instance :after ((self sequencer-module) &key)
  (let* ((play-button (make-instance 'button :text "▶" :x 5 :y 5))
         (add-track-button (make-instance 'button :text "+track" :x 35 :y 5)))
    (add-child self play-button)
    (add-child self add-track-button)
    (let ((sequencer self))
      (defmethod click ((self (eql play-button)) (button (eql 1)) x y)
        (if (.playing *audio*)
            (stop)
            (play)))
      (defmethod click ((self (eql add-track-button)) (button (eql 1)) x y)
        (add-new-track sequencer)))))

(defmethod render :after ((self sequencer-module) renderer)
  (let* ((first (car (.tracks self)))
         (last (car (last (.tracks self))))
         (x (+ (.absolute-x first) (* (.current-line self) *pixcel-per-line*))))
    (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
    (sdl2:render-draw-line renderer x (.absolute-y first) x
                           (+ (.absolute-y last) (.height last)))))

(defmethod resize :after ((self sequencer-module) xrel yrel)
  ;; TODO 小さくするとはみ出るし、拡大縮小もした方がいいかもしれない
  (loop for track in (.tracks self)
        do (resize track xrel 0)))

(defmethod drag-start ((self sequencer-module) x y (button (eql 3)))
  "sequencer-module-track にディスパッチする。"
  (awhen (child-module-at self x y)
    (drag-start it (- x (.x it)) (- y (.y it)) button)))

(defmethod drop ((self sequencer-module) (dropped drag-connect-mixin) x y (button (eql 3)))
  "sequencer-module-track にディスパッチする。"
  (awhen (child-module-at self x y)
    (drop it dropped (- x (.x it)) (- y (.y it)) button)))

(defun add-new-track (sequencer-module)
  (let* ((y (+ 13 *font-size* (* (length (.tracks sequencer-module))
                                 (1- *track-height*))))
         (track (make-instance 'sequencer-module-track :x 5 :y y)))
    (add-child sequencer-module track)
    (setf (.tracks sequencer-module)
          (append (.tracks sequencer-module) (list track)))
    track))

(defclass pattern-module (pattern module)
  ((pattern-editor :initform (make-instance 'pattern-editor) :accessor .pattern-editor))
  (:default-initargs :name "pattern"))

(defmethod initialize-instance :after ((self pattern-module) &key)
  (let ((pattern-editor (.pattern-editor self)))
    (add-child self pattern-editor)
    (setf (.pattern pattern-editor) self
          (.x pattern-editor) 5
          (.y pattern-editor) (+ 5 *font-size*)
          (.width pattern-editor) (- (.width self) 10)
          (.height pattern-editor) (- (.height self) (+ 10 *font-size*)))))

;; TODO audo.lisp の add-pattern と統合したくない？
(defmethod add-pattern ((track sequencer-module-track)
                        (pattern pattern-module)
                        start end)
  (let ((pattern-position (call-next-method)))
    (add-child track pattern-position)
    (setf (.x pattern-position) (* *pixcel-per-line* (.start pattern-position))
          (.y pattern-position) 2
          (.width pattern-position) (* *pixcel-per-line* (- (.end pattern-position)
                                            (.start pattern-position)))
          (.height pattern-position) (- (.height track) 4)
          (.name pattern-position) (.name pattern))
    pattern-position))

(defmethod mousebuttondown :before ((self pattern) button state clicks x y)
  (setf (.selected-pattern *app*) self))

(defmethod remove-pattern ((track sequencer-module-track)
                           (pattern-position pattern-position))
  (remove-child track pattern-position)
  (call-next-method))

(defmethod keydown ((self pattern-module) value scancode mod-value)
  (keydown (.pattern-editor self) value scancode mod-value))

(defmethod (setf .width) :after (value (self pattern-module))
  (setf (.width (.pattern-editor self)) (- (.width self) 10)))

(defmethod (setf .height) :after (value (self pattern-module))
  (setf (.height (.pattern-editor self)) (- (.height self) (+ 10 *font-size*))))

(defclass osc-module-mixin ()
  ((value-text :initform (make-instance 'text :value "0" :x 20 :y 20)
               :accessor .value-text)))

(defmethod initialize-instance :after ((self osc-module-mixin) &key)
  (add-child self (.value-text self)))

(defmethod render :before ((self osc-module-mixin) renderer)
  (setf (.value (.value-text self)) (format nil "~,5f" (.value self))))

(defclass sin-osc-module (sin-osc module osc-module-mixin)
  ()
  (:default-initargs :name "sin"))

(defclass saw-osc-module (saw-osc module osc-module-mixin)
  ()
  (:default-initargs :name "saw"))

(defclass adsr-module (adsr module)
  ()
  (:default-initargs :name "adsr" :height 95))

(defmethod initialize-instance :after ((self adsr-module) &key)
  (let ((x *layout-space*)
        (y (+ *font-size* *layout-space*))
        (width (- (.width self) (* 2 *layout-space*)))
        (height (+ *font-size* (round (/ *layout-space* 2)))))
    (add-child self
               (make-instance 'slider :value (lambda () (.a self))
                                      :x x :y y :width width :height height
                                      :onchange (lambda (x) (setf (.a self) x))))
    (add-child self
               (make-instance 'slider :value (lambda () (.d self))
                                      :x x
                                      :y (incf y (+ height (round (/ *layout-space* 2))))
                                      :width width :height height
                                      :onchange (lambda (x) (setf (.d self) x))))
    (add-child self
               (make-instance 'slider :value (lambda () (.s self))
                                      :x x
                                      :y (incf y (+ height (round (/ *layout-space* 2))))
                                      :width width :height height
                                      :onchange (lambda (x) (setf (.s self) x))))
    (add-child self
               (make-instance 'slider :value (lambda () (.r self))
                                      :x x
                                      :y (incf y (+ height (round (/ *layout-space* 2))))
                                      :width width :height height
                                      :onchange (lambda (x) (setf (.r self) x))))))

(defconstant +plugin-command-instrument+ 1)
(defconstant +plugin-command-effect+ 2)
(defconstant +plugin-command-manage+ 3)
(defconstant +plugin-command-edit+ 4)
(defconstant +plugin-command-quit+ 5)

(defclass plugin-module (audio-module module)
  ((plugin-description :initarg :plugin-description :accessor .plugin-description)
   (plugin-name :initarg :plugin-name :accessor .plugin-name)
   (host-process :accessor .host-process)
   (host-io :accessor .host-io)
   (out-buffer  :accessor .out-buffer
                :initform (make-array (* *frames-per-buffer* 9) :element-type 'unsigned-byte))
   (in-buffer  :accessor .in-buffer
                :initform (make-array (* *frames-per-buffer* 4) :element-type 'unsigned-byte))
   (left-buffer :initform (make-buffer) :accessor .left-buffer)
   (right-buffer :initform (make-buffer) :accessor .right-buffer)
   (mutex :initform (sb-thread:make-mutex) :accessor .mutex)))

(defclass instrument-plugin-module (plugin-module) ())
(defclass effect-plugin-module (plugin-module) ())

(defmethod initialize-instance :after ((self plugin-module) &key)
  (let ((button (make-instance 'button :text "Open" :x *layout-space* :y (+ *font-size* *layout-space*))))
    (add-child self button)
    (defmethod click ((button (eql button)) btn x y)
      (open-editor self)))
  (setf (.host-process self)
        (sb-ext:run-program *plugin-host-exe* (list (.plugin-name self)) :wait nil))
  (let ((pipe (sb-win32::create-named-pipe (format nil "~a~a" *plugin-host-pipe-name*
                                                   (sb-ext:process-pid (.host-process self)))
                                           sb-win32::pipe-access-duplex
                                           sb-win32::pipe-type-byte
                                           255 0 0 100 (cffi-sys::null-pointer))))
    (setf (.host-io self)
          (sb-sys:make-fd-stream pipe :input t :output t :element-type 'unsigned-byte))))

(defmethod close ((self plugin-module) &key abort)
  (declare (ignore abort))
  (let ((io (.host-io self)))
    (sb-thread:with-mutex ((.mutex self))
      (write-byte +plugin-command-quit+ io)
      (ignore-errors (force-output io))))
  (call-next-method))

(defmethod play-frame ((self instrument-plugin-module) midi-events frame)
  (let ((i -1)
        (length (length midi-events))
        (out (.out-buffer self))
        (in (.in-buffer self))
        (io (.host-io self))
        (left-buffer (.left-buffer self))
        (right-buffer (.right-buffer self)))
    (setf (aref out (incf i)) +plugin-command-instrument+)
    (setf (aref out (incf i)) (mod length #x100))
    (setf (aref out (incf i)) (mod (ash length -8) #x100))
    (loop for midi-event in midi-events
          do (setf (aref out (incf i)) (.event midi-event))
             (setf (aref out (incf i)) (.channel midi-event))
             (setf (aref out (incf i)) (.note midi-event))
             (setf (aref out (incf i)) (.velocity midi-event))
             (setf (aref out (incf i)) (mod (.frame midi-event) #x100))
             (setf (aref out (incf i)) (mod (ash (.frame midi-event) -8) #x100)))
    (sb-thread:with-mutex ((.mutex self))
      (write-sequence out io :end (1+ i))
      ;; Couldn't write to #<SB-SYS:FD-STREAM for "descriptor 2284" {1007B89EF3}>:
      ;; プロセスがパイプの他端を開くのを待っています。
      ;; [Condition of type SB-INT:SIMPLE-STREAM-ERROR]
      (force-output io)
      (loop for buffer in (list left-buffer right-buffer)
            do (let ((position (read-sequence in io)))
                 (declare (ignore position))
                 (loop for i below *frames-per-buffer*
                       do (setf (aref buffer i)
                                (coerce (ieee-floats:decode-float32
                                         (+ (aref in (* 4 i))
                                            (ash (aref in (+ (* 4 i) 1)) 8)
                                            (ash (aref in (+ (* 4 i) 2)) 16)
                                            (ash  (aref in (+ (* 4 i) 3)) 24)))
                                        'double-float))))))
    (route self left-buffer right-buffer)))

(defmethod play-frame ((self effect-plugin-module) left right)
  (let ((out (.out-buffer self))
        (in (.in-buffer self))
        (io (.host-io self))
        (left-buffer (.left-buffer self))
        (right-buffer (.right-buffer self)))
    (loop for lr in (list left right)
          with j = -1
          do (loop for i below *frames-per-buffer*
                   for n = (ieee-floats:encode-float32 (aref lr i))
                   do (setf (aref out (incf j)) (mod n #x100))
                      (setf (aref out (incf j)) (mod (ash n -8) #x100))
                      (setf (aref out (incf j)) (mod (ash n -16) #x100))
                      (setf (aref out (incf j)) (mod (ash n -24) #x100))))
    (sb-thread:with-mutex ((.mutex self))
      (write-byte +plugin-command-effect+ io)
      ;; TODO バッファの一部のみ read, write される対応
      ;; ここの force-output がないと write-byte したのが write-sequence した分と一緒に
      ;; 1 + 4095, 4096 とリードされ 1 バイト分読まれずに残ってしまう。
      (force-output io)
      (write-sequence out io :end (* *frames-per-buffer* 4 2))
      (force-output io)
      (loop for buffer in (list left-buffer right-buffer)
            do (let ((position (read-sequence in io :end (* *frames-per-buffer* 4))))
                 (declare (ignore position))
                 (loop for i below *frames-per-buffer*
                       do (setf (aref buffer i)
                                (coerce (ieee-floats:decode-float32
                                         (+ (aref in (* 4 i))
                                            (ash (aref in (+ (* 4 i) 1)) 8)
                                            (ash (aref in (+ (* 4 i) 2)) 16)
                                            (ash  (aref in (+ (* 4 i) 3)) 24)))
                                        'double-float))))))
    (route self left-buffer right-buffer)))

(defmethod print-object ((self plugin-module) stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a ~a"
              (.plugin-name self)
              (.host-process self))))

(defun open-editor (plugin-module)
  (sb-thread:with-mutex ((.mutex plugin-module))
    (write-byte +plugin-command-edit+ (.host-io plugin-module))
    (force-output (.host-io plugin-module))))

(defclass amp-module (amp module)
  ()
  (:default-initargs :name "amp"))

(defclass master-module (master module)
  ()
  (:default-initargs :name "master" :color (list #xff #xa5 #x00 *transparency*)))

(defclass menu-module (disable-drag-connect-mixin module)
  ((filter :initform "initialize-instance :after で上書く" :accessor .filter)
   (buttons :initform nil :accessor .buttons))
  (:default-initargs :width 300 :height 200 :name "Menu"))

(defmethod initialize-instance :after ((self menu-module) &key)
  (let ((button (make-instance 'button :text "Manage Plugins")))
    (add-child self button)
    (push button (.buttons self))
    (defmethod click ((button (eql button)) btn x y)
      (sb-ext:run-program *plugin-host-exe* nil :wait nil)
      (close self)))
  (loop for (name class) in `(("Pattern" pattern-module)
                              ("Sin" sin-osc-module)
                              ("Saw" saw-osc-module)
                              ("Adsr" adsr-module)
                              ("Amp" amp-module))
        do (let ((button (make-instance 'menu-builtin-button
                                        :text name
                                        :class class)))
             (add-child self button)
             (push button (.buttons self))))
  (loop for plugin-description in (load-known-plugins)
        do (let ((button (make-instance 'menu-plugin-button
                                        :text (.name plugin-description)
                                        :plugin-description plugin-description 
                                        :y (* (+ *font-size* 4)
                                              (length (.children self))))))
             (add-child self button)
             (push button (.buttons self))))
  (setf (.buttons self) (sort (.buttons self) #'string<
                              :key (lambda (x) (string-downcase (.label x)))))
  (setf (.filter self) ""))

(defmethod (setf .filter) :around (value (self menu-module))
  (when (not (equal (.filter self) value))
    (call-next-method)
    (let ((regex (ppcre:create-scanner
                  (format nil ".*~{~c~^.*~}" (coerce value 'list))
                  :case-insensitive-mode t)))
      (loop for button in (.buttons self)
            with i = 0
            if (ppcre:scan regex (.label button))
              do (add-child self button)
                 (setf (.y button) (* (+ *font-size* 4)
                                      (incf i)))
            else
              do (remove-child self button)))))

(defmethod keydown ((self menu-module) value scancode mod-value)
  (cond ((sdl2:scancode= scancode :scancode-escape)
         (close self))
        ((= value #x08)
         (setf (.filter self)
               (subseq (.filter self) 0 (max 0 (1- (length (.filter self)))))))
        ((<= value 127)
         (setf (.filter self) (format nil "~a~a" (.filter self) (code-char value))))))

(defmethod close ((self menu-module) &key abort)
  (declare (ignore abort))
  (when (eq self (.selected-module *app*))
    (setf (.selected-module *app*) nil))
  (remove-module self)
  (call-next-method))

(defclass menu-builtin-button (button)
  ((class :initarg :class :accessor .class)))

(defmethod click ((self menu-builtin-button) (button (eql 1)) x y)
  (add-module (make-instance (.class self) :name (.label self)
                                           :x (- (.mouse-x *app*) 10)
                                           :y (- (.mouse-y *app*) 10)))
  (close (.root-parent self)))

(defclass menu-plugin-button (button)
  ((plugin-description :initarg :plugin-description
                       :accessor .plugin-description)))

(defmethod click ((self menu-plugin-button) (button (eql 1)) x y)
  (let* ((plugin-description (.plugin-description self))
         (name (.name plugin-description))
         (class (if (.is-instrument plugin-description)
                    'instrument-plugin-module
                    'effect-plugin-module)))
    (add-module (make-instance class :name name
                                     :plugin-name name
                                     :x (- (.mouse-x *app*) 10)
                                     :y (- (.mouse-y *app*) 10)
                                     :plugin-description plugin-description))
    (close (.root-parent self))))

(defun open-menu ()
  (let ((module (make-instance 'menu-module
                               :x (- (.mouse-x *app*) 10)
                               :y (- (.mouse-y *app*) 10))))
    (add-module module)
    (setf (.selected-module *app*) module)))

(defclass plugin-description ()
  ((name :initarg :name :accessor .name)
   (format :initarg :format :accessor .format)
   (category :initarg :category :accessor .category)
   (manufacturer :initarg :manufacturer :accessor .manufacturer)
   (version :initarg :version :accessor .version)
   (file :initarg :file :accessor .file)
   (unique-id :initarg :unique-id :accessor .unique-id)
   (is-instrument :initarg :is-instrument :accessor .is-instrument)
   (num-inputs :initarg :num-inputs :accessor .num-inputs)
   (num-outputs :initarg :num-outputs :accessor .num-outputs)
   (uid :initarg :uid :accessor .uid)))

(defun load-known-plugins ()
  (let ((xml (cxml:parse-file (format nil "~a\\CoLiTrSynth\\Plugin Host.settings"
                                      (sb-ext:posix-getenv "APPDATA"))
                              (stp:make-builder)))
        (plugin-descriptions))
    (xpath:do-node-set (node (xpath:evaluate "/PROPERTIES/VALUE[@name=\"pluginList\"]/KNOWNPLUGINS/*" xml))
      (push (make-instance
             'plugin-description
             :name (xpath:string-value (xpath:evaluate "@name" node))
             :format (xpath:string-value (xpath:evaluate "@format" node))
             :category (xpath:string-value (xpath:evaluate "@category" node))
             :manufacturer (xpath:string-value (xpath:evaluate "@manufacturer" node))
             :version (xpath:string-value (xpath:evaluate "@version" node))
             :file (xpath:string-value (xpath:evaluate "@file" node))
             :unique-id (xpath:string-value (xpath:evaluate "@uniqueId" node))
             :is-instrument (equal (xpath:string-value (xpath:evaluate "@isInstrument" node))
                                   "1")
             :num-inputs (xpath:string-value (xpath:evaluate "@numInputs" node))
             :num-outputs (xpath:string-value (xpath:evaluate "@numOutputs" node))
             :uid (xpath:string-value (xpath:evaluate "@uid" node)))
            plugin-descriptions))
    plugin-descriptions))


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
              (sdl2-ttf:open-font font *font-size*)))

      (format t "Using SDL Library Version: ~D.~D.~D~%"
              sdl2-ffi:+sdl-major-version+
              sdl2-ffi:+sdl-minor-version+
              sdl2-ffi:+sdl-patchlevel+)
      (finish-output)

      (sdl2:with-window (win :title "CoLiTrSynth" :w (.width *app*) :h (.height *app*)
                             :flags '(:resizable)
                         :x 1 :y 25)    ;デバッグするのにこの位置が楽
        (setf (.win *app*) win)
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:hide-window win)
          (sdl2:show-window win)
          (format t "Beginning main loop.~%")
          (finish-output)
          (with-audio
            ;;(make-default-modules)
            ;; (make-test-modules)
            (make-plugin-test-modules)
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
                     t))))))))

(defun make-default-modules ()
  (let* ((sequencer (.sequencer *audio*))
         (master (.master *audio*)))
    (add-new-track sequencer)
    (add-new-track sequencer)
    (setf (.modules *app*)
          (list sequencer master))))

(defun make-plugin-test-modules ()
  (let* ((line-length 8)
         (sequencer (.sequencer *audio*))
         (track1 (add-new-track sequencer))
         (plugin (make-instance 'instrument-plugin-module
                                :plugin-name "Zebralette"
                                :name "Zebralette" :x 200 :y 250))
         (master (.master *audio*))
         (pattern1 (make-instance 'pattern-module
                                  :name "Pattern1"
                                  :length line-length
                                  :lines (list-to-pattern-lines
                                          (list a4 e4 none g4
                                                a4 off  g4 c4))
                                  :x 5  :y 250 :height 200)))
    (connect track1 plugin)
    (connect plugin master)
    (add-pattern track1 pattern1 0 line-length)
    (setf (.modules *app*)
          (list sequencer master
                pattern1 plugin ;; reverb
                ))))

(defun make-test-modules ()
  (let* ((line-length 8)
         (sequencer (.sequencer *audio*))
         (track1 (add-new-track sequencer))
         (track2 (add-new-track sequencer))
         (master (.master *audio*))
         (pattern1 (make-instance 'pattern-module
                                  :name "plack"
                                  :length line-length
                                  :lines (list-to-pattern-lines
                                          (list a4 e4 none g4
                                                a4 off  g4 c4))
                                  :x 5  :y 250 :height 200))
         (osc1 (make-instance 'sin-osc-module :x 245 :y 300))
         (adsr1 (make-instance 'adsr-module :d 0.2d0 :s 0d0
                                            :x 365 :y 250))
         (amp1 (make-instance 'amp-module
                              :x 485 :y 250))
         (pattern2 (make-instance 'pattern-module
                                  :name "brass"
                                  :length line-length
                                  :lines (list-to-pattern-lines
                                          (list a3 e3 none g3
                                                a3 off  g3 c3))
                                  :x 125 :y 250 :height 200))
         (osc2 (make-instance 'saw-osc-module
                              :x 245 :y 400))
         (adsr2 (make-instance 'adsr-module :d 0.7d0 :s 0.8d0
                                            :x 365 :y 350))
         (amp2 (make-instance 'amp-module
                              :x 485 :y 350)))
    (connect track1 osc1)
    (connect track1 adsr1)
    (connect osc1 amp1)
    (connect adsr1 amp1)
    (connect amp1 master)
    (connect track2 osc2)
    (connect track2 adsr2)
    (connect osc2 amp2)
    (connect adsr2 amp2)
    (connect amp2 master)
    (add-pattern track1 pattern1 0 line-length)
    (add-pattern track1 pattern1 line-length (* 2 line-length))
    (add-pattern track2 pattern2 line-length (* 2 line-length))
    (add-pattern track1 pattern1 (* 2 line-length) (* 3 line-length))
    (add-pattern track2 pattern2 (* 2 line-length) (* 3 line-length))
    (setf (.modules *app*)
          (list sequencer master
                pattern1 osc1 adsr1 amp1 
                pattern2 osc2 adsr2 amp2))))

(defun handle-sdl2-keydown-event (keysym)
  (let ((value (sdl2:sym-value keysym))
        (scancode (sdl2:scancode-value keysym))
        (mod-value (sdl2:mod-value keysym)))
    #+nil
    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
            (sdl2:sym-value keysym)
            scancode
            mod-value)
    (aif (.selected-module *app*)
         (keydown it value scancode mod-value)
         (cond ((sdl2:scancode= scancode :scancode-f)
                (open-menu))))))

(defun handle-sdl2-keyup-event (keysym)
  (let  ((value (sdl2:sym-value keysym))
         (scancode (sdl2:scancode-value keysym))
         (mod-value (sdl2:mod-value keysym)))
    (keyup (.selected-module *app*) value scancode mod-value)))

(defun handle-sdl2-mousemotion-event (x y xrel yrel state)
  #+nil
  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
          x xrel y yrel state)
  (setf (.mouse-x *app*) x)
  (setf (.mouse-y *app*) y)
  (let ((module (or (.target (.drag-state *app*))
                    (.drag-resize-module *app*)
                    (module-at-mouse *app*))))
    (mousemotion module
                 (- x (.absolute-x module)) (- y (.absolute-y module))
                 xrel yrel state)))

(defun handle-sdl2-mousebuttondown-event (button state clicks x y)
  #+nil
  (format t "Mouse button down button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (let ((module (module-at-mouse *app*)))
    (mousebuttondown module
                     button state clicks
                     (- x (.absolute-x module)) (- y (.absolute-y module)))))

(defun handle-sdl2-mousebuttonup-event (button state clicks x y)
  #+nil
  (format t "Mouse button up button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (awhen (or (let ((drag-state (.drag-state *app*)))
               (and drag-state
                    (.dragging drag-state)
                    (.target drag-state)))
             (and (.dragging *app*)
                  (.drag-resize-module *app*))
             (module-at-mouse *app*))
    (mousebuttonup it button state clicks
                   (- x (.absolute-x it)) (- y (.absolute-y it))))
  (setf (.drag-resize-module *app*) nil)
  (setf (.dragging *app*) nil)
  (setf (click-target-module button) nil)
  (setf (.connect-from-module *app*) nil))

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
  (loop for module in (.modules *app*)
        do (close module))
  (when (.font *app*)
    (sdl2-ttf:close-font (.font *app*))
    (setf (.font *app*) nil))
  (when (= 1 (sdl2-ttf:was-init))
    (sdl2-ttf:quit)))
