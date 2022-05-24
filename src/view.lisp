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

;;;; 処理の都合上必要なこ
(defvar *pattern-scroll-lock* nil)
(defvar *pattern-line-index*)

(defconstant +mouse-button-count+ 16)
(defconstant +column-width+ 7)


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
  (:method (self value scancode mod-value)
    (cond ((sdl2:scancode= scancode :scancode-scrolllock)
           (setf *pattern-scroll-lock* (not *pattern-scroll-lock*)))
          ((sdl2:scancode= scancode :scancode-space )
              (if (.playing *audio*)
                  (stop)
                  (play)))
          ((sdl2:scancode= scancode :scancode-f)
           (open-menu)))))

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

(defclass view ()
  ((parent :initarg :parent :initform nil :accessor .parent)
   (children :initarg :children :initform nil :accessor .children)))

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

(defmethod .root-parent ((self view))
  (aif (.parent self)
       (.root-parent it)
       self))

(defmethod mousebuttondown ((self view) button state clicks x y)
  (call-next-method)
  (awhen (child-module-at self x y)
    (mousebuttondown it button state clicks
                     (- x (.x it)) (- y (.y it)))))

(defmethod mousebuttonup ((self view) button state clicks x y)
  (call-next-method)
  (awhen (child-module-at self x y)
    (mousebuttonup it button state clicks
                   (- x (.x it)) (- y (.y it)))))

(defmethod click ((self view) button x y)
  (call-next-method)
  (awhen (child-module-at self x y)
    (click it button (- x (.x it)) (- y (.y it)))))

(defmethod drop ((self view) dropped x y button)
  (call-next-method)
  (awhen (child-module-at self x y)
    (drop it dropped (- x (.x it)) (- y (.y it)) button)))

(defmethod mousemotion ((self view) x y xrel yrel state)
  (call-next-method)
  (awhen (child-module-at self x y)
   (mousemotion it (- x (.x it)) (- y (.y it))
                xrel yrel state)))

(defmethod move ((self view) xrel yrel)
    (incf (.x self) xrel)
    (incf (.y self) yrel))

(defmethod resize ((self view) xrel yrel)
  (setf (.width self) (max 20 (+ (.width self) xrel)))
  (setf (.height self) (max 20 (+ (.height self) yrel))))

(defmethod add-child ((parent view) (child view))
  (unless (eq parent (.parent child))
    (setf (.children parent) (append (.children parent) (list child)))
    (setf (.parent child) parent)))

(defmethod remove-child ((parent view) (child view))
  (setf (.children parent) (remove child (.children parent)))
  (setf (.parent child) nil))

(defmethod .x ((self null))
  0)

(defmethod .absolute-x ((self null))
  0)

(defmethod .absolute-x ((self view))
  (+ (.x self)
     (.absolute-x (.parent self))))

(defmethod .absolute-center-x ((self view))
  (+ (.absolute-x self)
     (round (/ (.width self) 2))))

(defmethod .y ((self null))
  0)

(defmethod .absolute-y ((self null))
  0)

(defmethod .absolute-center-y ((self view))
  (+ (.absolute-y self)
     (round (/ (.height self) 2))))

(defmethod .absolute-y ((self view))
  (+ (.y self)
     (.absolute-y (.parent self))))

(defmethod render ((self view) renderer)
  (apply #'sdl2:set-render-draw-color renderer (.color self))
  (sdl2:render-draw-rect renderer
                         (sdl2:make-rect (.absolute-x self)
                                         (.absolute-y self)
                                         (.width self)
                                         (.height self)))
  (loop for child in (.children self)
        do (render child renderer))
  (call-next-method))

(defmethod render ((self model) renderer)
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

(defclass module (drag-resize-mixin
                  drag-move-mixin
                  drag-connect-mixin
                  view)
  ((model :initarg :model :accessor .model)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-mop:finalize-inheritance (find-class 'model)))
(delegate-model)

(defmethod close ((self module) &key abort)
  (declare (ignore abort)))

(defmethod keydown ((self module) value scancode mod-value)
  (if (and (sdl2:scancode= scancode :scancode-delete)
           (not (zerop (logand mod-value sdl2-ffi:+kmod-shift+)))
           (not (zerop (logand mod-value sdl2-ffi:+kmod-ctrl+)))
           (not (eq self (.sequencer *audio*)))
           (not (eq self (.master *audio*))))
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

(defmethod connect ((in drag-connect-mixin) (out drag-connect-mixin))
  (connect (.model in) (.model out)))

(defmethod disconnect ((in drag-connect-mixin) (out drag-connect-mixin))
  (disconnect (.model in) (.model out)))

(defmethod disconnect-all ((self module))
  (disconnect-all (.model self)))

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

(defclass disable-drag-connect-mixin ()
  ())

(defmethod drag-start ((self disable-drag-connect-mixin) x y (button (eql 3))))

(defmethod drop ((self disable-drag-connect-mixin) dropped x y (button (eql 3))))

(defclass text (function-value-mixin view renderable)
  ((last-value :initform "" :accessor .last-value)
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

(defclass button (view renderable)
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
                  function-value-mixin drag-mixin view renderable)
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

(defclass pattern-editor (view renderable)
  ((pattern :accessor .pattern)
   (lines :initform nil :accessor .lines)
   (cursor-x :initform 0 :accessor .cursor-x)
   (cursor-y :initform 0 :accessor .cursor-y)
   (octave :initform 4 :accessor .octave)
   (edit-step :initform 0 :accessor .edit-step)))

(defmethod render :before ((self pattern-editor) renderer)
  (when (and (.playing *audio*)
             (not *pattern-scroll-lock*))
    (setf (.cursor-y self) (.current-line (.pattern self))))
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
        ;; PERFORMANCE 毎回やる必要ないよね
        (loop for pattern-line across pattern-lines
              for pattern-editor-line in (.lines self)
              do (setf (.line pattern-editor-line) pattern-line)))))

(defmethod render ((self pattern-editor) renderer)
  (let ((texture (multiple-value-call
                     #'sdl2:create-texture renderer :rgba8888 :target
                   (multiple-value-call
                       ;; window からはみ出ると表示がひずむので
                       (lambda (w h) (values (+ w (.width self))
                                             ;; 最後の方にスクロールしたとき表示がひずむので
                                             (+ h (.height self) (.height self))))
                     (sdl2:get-window-size (.win *app*))))))
    (unwind-protect
         (let ((play-x (.absolute-x self))
               (play-y (+ (* (.current-line (.pattern self)) *char-height*) (.absolute-y self) 2))
               (play-w (.width self))
               (play-h *char-height*)
               (cursor-x (+ (* *char-width* (+ 3 (.cursor-x self)))  (.absolute-x self) 2))
               (cursor-y (+ (* (.cursor-y self) *char-height*) (.absolute-y self) 2))
               (cursor-w (if (zerop (mod (.cursor-x self) +column-width+))
                             (* *char-width* 3)
                             *char-width*))
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
                 for i from 0
                 do (let ((*pattern-line-index* i))
                      (render child renderer)))
           (sdl2:set-render-target renderer nil)
           (let* ((dst-x (.absolute-x self))
                  (dst-y (.absolute-y self))
                  (dst-w (.width self))
                  (dst-h (.height self))
                  (offset-y (- cursor-y dst-y (round (/ dst-h 2)) (- (round (/ *char-height* 2)))))
                  (src-x dst-x)
                  (src-y (+ dst-y offset-y))
                  (src-w dst-w)
                  (src-h dst-h)
                  (dst-rect (sdl2:make-rect dst-x dst-y dst-w dst-h))
                  (src-rect (sdl2:make-rect src-x src-y src-w src-h)))
             (sdl2:render-copy renderer texture :source-rect src-rect :dest-rect dst-rect)))
      (sdl2:destroy-texture texture))))

(defmethod keydown ((self pattern-editor) value scancode mod-value)
  (let* ((shift-p (not (zerop (logand mod-value sdl2-ffi:+kmod-shift+))))
         (ctrl-p (not (zerop (logand mod-value sdl2-ffi:+kmod-ctrl+))))
         (max-cursor-y (1- (length (.lines (.pattern self)))))
         (lines (.lines (.pattern self)))
         (line (aref lines (.cursor-y self)))
         (cursor-x (.cursor-x self)))
    (labels ((step-next ()
               (setf (.cursor-y self) (mod (+ (.cursor-y self) (.edit-step self))
                                           (1+ max-cursor-y))))
             (on-note ()
               (zerop (mod cursor-x +column-width+)))
             (set-note (note)
               (when (on-note)
                 (setf (.note (aref (.columns line) (floor (/ cursor-x +column-width+)))) note)
                 (step-next)))
             (set-velocity (velocity)
               (case (mod cursor-x +column-width+)
                 (4 (set-velocity-x0 velocity))
                 (5 (set-velocity-0x velocity))))
             (set-velocity-x0 (velocity)
               (when (<= velocity 7)
                 (let* ((column (aref (.columns line) (floor (/ cursor-x +column-width+)))))
                   (setf (.velocity column)
                         (+ (* velocity #x10)
                            (mod (.velocity column) #x10))))
                 (step-next)))
             (set-velocity-0x (velocity)
               (let ((column (aref (.columns line) (floor (/ cursor-x +column-width+)))))
                (setf (.velocity column)
                      (+ (logand (.velocity column) #xf0)
                         velocity)))
               (step-next)))
      (cond ((or (sdl2:scancode= scancode :scancode-up)
                 (sdl2:scancode= scancode :scancode-k))
             (when (minusp (decf (.cursor-y self)))
               (setf (.cursor-y self) max-cursor-y)))
            ((or (sdl2:scancode= scancode :scancode-down)
                 (sdl2:scancode= scancode :scancode-j))
             (when (< max-cursor-y (incf (.cursor-y self)))
               (setf (.cursor-y self) 0)))
            ((and (or (sdl2:scancode= scancode :scancode-left)
                      (sdl2:scancode= scancode :scancode-h))
                  ctrl-p shift-p)
             (loop with length = (max 1 (1- (.length line)))
                   for line across lines
                   do (setf (.length line) length)))
            ((or (sdl2:scancode= scancode :scancode-left)
                 (sdl2:scancode= scancode :scancode-h))
             (let* ((value (- (print cursor-x) (case (mod cursor-x +column-width+)
                                           (0 2)
                                           (4 4)
                                           (5 1)
                                           (t 0)))))
               (when (<= 0 value)
                 (setf (.cursor-x self) (print value)))))
            ((and (or (sdl2:scancode= scancode :scancode-right)
                      (sdl2:scancode= scancode :scancode-l))
                  ctrl-p shift-p)
             (loop with length = (min (1+ (.length line)) (length (.columns line)))
                   for line across lines
                   do (setf (.length line) length)))
            ((or (sdl2:scancode= scancode :scancode-right)
                 (sdl2:scancode= scancode :scancode-l))
             (let* ((value (+ cursor-x (case (mod cursor-x 7)
                                         (0 4)
                                         (4 1)
                                         (5 2)
                                         (t 0)))))
               (when (< value (* 7 (.length line)))
                 (setf (.cursor-x self) value))))
            ((sdl2:scancode= scancode :scancode-0)
             (set-note (+ d#1 (* 12 (.octave self))))
             (set-velocity 0))
            ((sdl2:scancode= scancode :scancode-1)
             (cond (ctrl-p
                    (when (< 0 (.octave self))
                      (decf (.octave self))))
                   (t (set-velocity 1))))
            ((and (sdl2:scancode= scancode :scancode-2) ctrl-p)
             (when (< (.octave self) 9)
               (incf (.octave self))))
            ((sdl2:scancode= scancode :scancode-2)
             (set-note (+ c#0 (* 12 (.octave self))))
             (set-velocity 2))
            ((and (sdl2:scancode= scancode :scancode-3) ctrl-p shift-p)
             (when (< 0 (.edit-step self))
               (decf (.edit-step self))))
            ((and (sdl2:scancode= scancode :scancode-3) ctrl-p)
             (setf (.edit-step self) (floor (/ (.edit-step self) 2))))
            ((sdl2:scancode= scancode :scancode-3)
             (set-note (+ d#0 (* 12 (.octave self))))
             (set-velocity 3))
            ((and (sdl2:scancode= scancode :scancode-4) ctrl-p shift-p)
             (incf (.edit-step self)))
            ((and (sdl2:scancode= scancode :scancode-4) ctrl-p)
             (setf (.edit-step self)
                   (if (zerop (.edit-step self))
                       1
                       (* (.edit-step self) 2))))
            ((and (sdl2:scancode= scancode :scancode-4))
             (set-velocity 4))
            ((sdl2:scancode= scancode :scancode-5)
             (set-note (+ f#0 (* 12 (.octave self))))
             (set-velocity 5))
            ((sdl2:scancode= scancode :scancode-6)
             (set-note (+ g#0 (* 12 (.octave self))))
             (set-velocity 6))
            ((sdl2:scancode= scancode :scancode-7)
             (set-note (+ a#0 (* 12 (.octave self))))
             (set-velocity 7))
            ((sdl2:scancode= scancode :scancode-8)
             (set-velocity 8))
            ((sdl2:scancode= scancode :scancode-9)
             (set-note (+ c#1 (* 12 (.octave self))))
             (set-velocity 9))            
            ((sdl2:scancode= scancode :scancode-a)
             (set-note off)
             (set-velocity #x0a))
            ((sdl2:scancode= scancode :scancode-b)
             (set-velocity #x0b))
            ((sdl2:scancode= scancode :scancode-c)
             (set-velocity #x0c))
            ((sdl2:scancode= scancode :scancode-d)
             (set-velocity #x0d))
            ((sdl2:scancode= scancode :scancode-e)
             (set-note (+ e0 (* 12 (.octave self))))
             (set-velocity #x0e))
            ((sdl2:scancode= scancode :scancode-f)
             (set-velocity #x0f))
            ((sdl2:scancode= scancode :scancode-q)
             (set-note (+ c0 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-w)
             (set-note (+ d0 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-r)
             (set-note (+ f0 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-t)
             (set-note (+ g0 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-y)
             (set-note (+ a0 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-u)
             (set-note (+ b0 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-i)
             (set-note (+ c1 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-o)
             (set-note (+ d1 (* 12 (.octave self)))))
            ((sdl2:scancode= scancode :scancode-p)
             (set-note (+ e1 (* 12 (.octave self)))))
            ((= scancode 47)             ;@
             (set-note (+ f1 (* 12 (.octave self)))))
            ((= scancode 46)             ;^
             (set-note (+ f#1 (* 12 (.octave self)))))
            ((= scancode 48)             ;[
             (set-note (+ g1 (* 12 (.octave self)))))
            ((= scancode 137)            ;\
             (set-note (+ g#1 (* 12 (.octave self)))))
            ((and (sdl2:scancode= scancode :scancode-delete)
                  (or (not shift-p) (not ctrl-p)))
             (set-note none))
            (t 'call-next-method)))))

(defclass pattern-editor-line (text)
  ((line :initarg :line :accessor .line)))

(defmethod render :before ((self pattern-editor-line) renderer)
  (setf (.value self)
        (with-output-to-string (out)
          (format out "~2,'0X" *pattern-line-index*)
          (loop with line = (.line self)
                repeat (.length line)
                for column across (.columns line)
                for note = (.note column)
                do (cond ((= note off)
                          (write-string " OFF --" out))
                         ((= note none)
                          (write-string " --- --" out))
                         (t
                          (let* ((c-s-o (format nil "~a" (midino-to-note note)))
                                 (c (char c-s-o 0))
                                 (s (if (char= (char c-s-o 1) #\#)
                                        #\#
                                        #\-))
                                 (o (char c-s-o (if (char= s #\#) 2 1))))
                            (format out " ~c~c~c ~2,'0X"
                                    c s o (.velocity column)))))))))

(defparameter *track-height* 40)        ;TODO 固定長で妥協

(defclass track-view (drag-mixin
                      drag-connect-mixin
                      drop-mixin
                      view
                      renderable)
  ((model :initarg :model :initform (make-instance 'track) :accessor .model))
  (:default-initargs :width 690 :height *track-height*))

(defmethod drop ((self track-view) (dropped drag-connect-mixin) x y (button (eql 3))))

(defun pixcel-to-line (pixcel)
  (* (round (/ pixcel *pixcel-per-line*) 4) 4))

(defun line-to-pixcel (line)
  (* *pixcel-per-line* line))

(defmethod click ((self track-view) button x y)
  (case button
    (1
     (let ((module (.selected-pattern *app*)))
       (if (typep module 'pattern-module)
           (let* ((start (pixcel-to-line x))
                  (end (+ start (.length module))))
             (when (every (lambda (x)
                            (or (<= end (.start x))
                                (<= (.end x) start)))
                          (.pattern-positions (.model self)))
               (add-pattern self module start end)))
           (call-next-method))))
    (3
     (awhen (child-module-at self x y)
       (remove-pattern self it))
     (call-next-method))))

(defclass pattern-position-view (drag-mixin
                                 name-mixin
                                 view
                                 renderable)
  ((model :initarg :model :accessor .model)
   (move-delta-x :initform 0 :accessor .move-delta-x)))

(defmethod drag ((self pattern-position-view) xrel yrel button)
  (let* ((pixcel (+ (.x self) (.move-delta-x self) xrel))
         (line (pixcel-to-line pixcel))
         (rounded-pixcel (line-to-pixcel line)))
    (setf (.x self) rounded-pixcel
          (.move-delta-x self) (- pixcel rounded-pixcel))))

(defmethod drag-end ((self pattern-position) x y button)
  (setf (.move-delta-x self) 0)
  (call-next-method))

(defmethod drop ((self track-view) (pattern-position-view pattern-position-view) x y button)
  (let ((delta (- (pixcel-to-line (.x pattern-position-view)) (.start pattern-position-view))))
    (incf (.start pattern-position-view) delta)
    (incf (.end pattern-position-view) delta)
    (update-sequencer-end)))

(defclass sequencer-module (module)
  ((track-views :initform nil :accessor .track-views))
  (:default-initargs :model (make-instance 'sequencer)))

(defmethod initialize-instance :after ((self sequencer-module) &key)
  (let* ((play-button (make-instance 'button :text "▶" :x 5 :y *layout-space*))
         (add-track-button (make-instance 'button :text "+track" :x 35 :y *layout-space*))
         (bpm (make-instance 'text
                             :value (lambda ()
                                      (format nil "BPM ~f" (.bpm (.model self))))
                             :x 100 :y *layout-space*)))
    (add-child self play-button)
    (add-child self add-track-button)
    (add-child self bpm)
    (let ((sequencer self))
      (defmethod click ((self (eql play-button)) (button (eql 1)) x y)
        (if (.playing *audio*)
            (stop)
            (play)))
      (defmethod click ((self (eql add-track-button)) (button (eql 1)) x y)
        (add-new-track sequencer)))))

(defmethod keydown ((self sequencer-module) value scancode mod-value)
  (cond ((sdl2:scancode= scancode :scancode-1)
         (decf (.bpm (.model self))))
        ((sdl2:scancode= scancode :scancode-2)
         (incf (.bpm (.model self))))
        (t (call-next-method))))

(defmethod render :after ((self sequencer-module) renderer)
  (let* ((first (car (.track-views self)))
         (last (car (last (.track-views self))))
         (x (+ (.absolute-x first)
               (* (.current-line (.model self))
                  *pixcel-per-line*))))
    (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
    (sdl2:render-draw-line renderer x (.absolute-y first) x
                           (+ (.absolute-y last) (.height last)))))

(defmethod resize :after ((self sequencer-module) xrel yrel)
  ;; TODO 小さくするとはみ出るし、拡大縮小もした方がいいかもしれない
  (loop for track in (.tracks self)
        do (resize track xrel 0)))

(defmethod drag-start ((self sequencer-module) x y (button (eql 3)))
  "track-view にディスパッチする。"
  (awhen (child-module-at self x y)
    (drag-start it (- x (.x it)) (- y (.y it)) button)))

(defmethod drop ((self sequencer-module) (dropped drag-connect-mixin) x y (button (eql 3)))
  "track-view にディスパッチする。"
  (awhen (child-module-at self x y)
    (drop it dropped (- x (.x it)) (- y (.y it)) button)))

(defmethod add-new-track ((self sequencer-module))
  (let* ((track (add-new-track (.model self)))
         (y (+ 13 *font-size* (* (length (.tracks (.model self)))
                                 (1- *track-height*))))
         (track-view (make-instance 'track-view :x 5 :y y :model track)))
    (add-child self track-view)
    (setf (.track-views self) (append (.track-views self) (list track-view)))
    track-view))

(defclass pattern-module (module)
  ((pattern-editor :accessor .pattern-editor
                   :initform (make-instance 'pattern-editor)))
  (:default-initargs :model (make-instance 'pattern)))

(defmethod initialize-instance :after ((self pattern-module) &key)
  (let* ((pattern-editor (.pattern-editor self))
         (octave (make-instance 'text :value (lambda ()
                                               (format nil "~d" (.octave pattern-editor)))
                                      :x (- (.width self) (* *char-width* 4) *layout-space*)
                                      :y *layout-space*))
         (edit-step (make-instance 'text :value (lambda ()
                                                  (format nil "~2,'0d" (.edit-step pattern-editor)))
                                         :x (- (.width self) (* *char-width* 2) *layout-space*)
                                         :y *layout-space*)))
    (add-child self pattern-editor)
    (add-child self octave)
    (add-child self edit-step)
    (setf (.pattern pattern-editor) (.model self)
          (.x pattern-editor) 5
          (.y pattern-editor) (+ 5 *font-size*)
          (.width pattern-editor) (- (.width self) 10)
          (.height pattern-editor) (- (.height self) (+ 10 *font-size*)))))

(defmethod add-pattern ((track track-view)
                        (pattern pattern-module)
                        start end)
  (let* ((pattern-position (add-pattern (.model track) (.model pattern) start end))
         (view (make-instance 'pattern-position-view :model pattern-position)))
    (add-child track view)
    (setf (.x view) (* *pixcel-per-line* (.start pattern-position))
          (.y view) 2
          (.width view) (* *pixcel-per-line* (- (.end pattern-position)
                                            (.start pattern-position)))
          (.height view) (- (.height track) 4))))

(defmethod close ((self pattern-module) &key abort)
  (declare (ignore abort))
  (loop for track-view in (.children *sequencer-module*)
        if (typep track-view 'track-view)
          do (loop for pattern-position-view in (.children track-view)
                   
                   if (and (typep pattern-position-view 'pattern-position-view)
                           (eql (.model self) (.pattern (.model pattern-position-view))))
                   do (remove-pattern track-view pattern-position-view))))

(defmethod mousebuttondown :before ((self pattern) button state clicks x y)
  (setf (.selected-pattern *app*) self))

(defmethod remove-pattern ((track track-view)
                           (pattern-position-view pattern-position-view))
  (remove-child track pattern-position-view)
  (remove-pattern (.model track) (.model pattern-position-view)))

(defmethod keydown ((self pattern-module) value scancode mod-value)
  (when (eq 'call-next-method
            (keydown (.pattern-editor self) value scancode mod-value))
    (call-next-method)))

(defmethod (setf .width) :after (value (self pattern-module))
  (setf (.width (.pattern-editor self)) (- (.width self) 10)))

(defmethod (setf .height) :after (value (self pattern-module))
  (setf (.height (.pattern-editor self)) (- (.height self) (+ 10 *font-size*))))

(defclass osc-module-mixin ()
  ())

(defmethod initialize-instance :after ((self osc-module-mixin) &key)
  (let ((value-text (make-instance
                     'text :x 20 :y 20
                     :value (lambda () (format nil "~,5f" (.value (.model self)))))))
    (add-child self value-text)))

(defclass sin-osc-module (module osc-module-mixin)
  ()
  (:default-initargs :name "sin"
                     :model (make-instance 'sin-osc)))

(defclass saw-osc-module (module osc-module-mixin)
  ()
  (:default-initargs :name "saw"
                     :model (make-instance 'saw-osc)))

(defclass adsr-module (module)
  ()
  (:default-initargs :name "adsr" :height 95
                     :model (make-instance 'adsr)))

(defmethod initialize-instance :after ((self adsr-module) &key)
  (let ((x *layout-space*)
        (y (+ *font-size* *layout-space*))
        (width (- (.width self) (* 2 *layout-space*)))
        (height (+ *font-size* (round (/ *layout-space* 2))))
        (adsr (.model self)))
    (add-child self
               (make-instance 'slider :value (lambda () (.a adsr))
                                      :x x :y y :width width :height height
                                      :onchange (lambda (x) (setf (.a adsr) x))))
    (add-child self
               (make-instance 'slider :value (lambda () (.d adsr))
                                      :x x
                                      :y (incf y (+ height (round (/ *layout-space* 2))))
                                      :width width :height height
                                      :onchange (lambda (x) (setf (.d adsr) x))))
    (add-child self
               (make-instance 'slider :value (lambda () (.s adsr))
                                      :x x
                                      :y (incf y (+ height (round (/ *layout-space* 2))))
                                      :width width :height height
                                      :onchange (lambda (x) (setf (.s adsr) x))))
    (add-child self
               (make-instance 'slider :value (lambda () (.r adsr))
                                      :x x
                                      :y (incf y (+ height (round (/ *layout-space* 2))))
                                      :width width :height height
                                      :onchange (lambda (x) (setf (.r adsr) x))))))

(defclass plugin-module (module)
  ())

(defclass instrument-plugin-module (plugin-module)
  ())

(defclass effect-plugin-module (plugin-module)
  ())

(defmethod initialize-instance :after ((self plugin-module) &key)
  (let ((button (make-instance 'button :text "Open" :x *layout-space* :y (+ *font-size* *layout-space*))))
    (add-child self button)
    (defmethod click ((button (eql button)) btn x y)
      (open-editor (.model self))))
  (run-plugin-host (.model self)))

(defmethod close ((self plugin-model) &key abort)
  (close (.model self) :abort abort)
  (call-next-method))

(defclass amp-module (module)
  ()
  (:default-initargs :name "amp"
                     :model (make-instance 'amp)))

(defclass master-module (master module)
  ()
  (:default-initargs 
                     :model (make-instance 'master)))

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
         (class (if (.is-instrument plugin-description)
                    (list 'instrument-plugin-module 'instrument-plugin-model)
                    (list 'effect-plugin-module 'effect-plugin-module))))
    (add-module (make-instance
                 (car class)
                 :model
                 (make-instance (cadr class)
                                :x (- (.mouse-x *app*) 10)
                                :y (- (.mouse-y *app*) 10)
                                :plugin-description plugin-description)))
    (close (.root-parent self))))

(defun open-menu ()
  (let ((module (make-instance 'menu-module
                               :x (- (.mouse-x *app*) 10)
                               :y (- (.mouse-y *app*) 10))))
    (add-module module)
    (setf (.selected-module *app*) module)))

