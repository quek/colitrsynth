(in-package :colitrsynth)

(defparameter *plugin-host-exe* "C:/Users/ancient/Documents/Visual Studio 2022/PluginHost/Builds/VisualStudio2022/x64/Debug/App/PluginHost.exe")
(defparameter *plugin-host-pipe-name* "\\\\.\\pipe\\pluin-host")

;;;; 処理の都合上必要なこ
(defvar *pattern-scroll-lock* nil)
(defvar *pattern-line-index*)
(defparameter *track-height* 30)        ;TODO 固定長で妥協

(defconstant +mouse-button-count+ 16)
(defconstant +column-width+ 7)

(defgeneric make-module (model))

(defgeneric initialize (module))

(defgeneric render (self renderer)
  (:method (self renderer)))

(defgeneric render-connection (self r)
  (:method (self r)))

(defgeneric resized (self)
  (:method (self)))

(defgeneric mousebuttondown (self button state clicks x y)
  (:method (self button state clicks x y)
    (setf (click-target-module button) self)
    (swhen (.focused-view *app*)
      (unless (and (<= (.absolute-x it) (.mouse-x *app*) (+ (.absolute-x it) (.width it)))
                   (<= (.absolute-y it) (.mouse-y *app*) (+ (.absolute-y it) (.height it))))
        (setf it nil)))))

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
           (open-menu))
          ((and (sdl2:scancode= scancode :scancode-o)
                (not (zerop (logand mod-value sdl2-ffi:+kmod-ctrl+))))
           (sb-thread:make-thread (lambda (*app*)
                                    (awhen (get-open-file-name)
                                      (sb-concurrency:send-message
                                       (.mbox *app*)
                                       (lambda () (open-song it)))))
                                  :arguments (list *app*)))
          ((and (sdl2:scancode= scancode :scancode-s)
                (not (zerop (logand mod-value sdl2-ffi:+kmod-ctrl+))))
           (sb-thread:make-thread
            (lambda (*app*)
              (let ((file (or (.song-file *app*)
                              (get-save-file-name))))
                (when file
                  (unless (alexandria:ends-with ".lisp" file)
                    (setf file (format nil "~a.lisp" file)))
                  (sb-concurrency:send-message
                   (.mbox *app*)
                   (lambda () (save-song file))))))
            :arguments (list *app*))))))

(defgeneric keyup (self value scancode mod-value)
  (:method (self value scancode mod-value)))

(defgeneric move (self xrel yrel)
  (:method (self xrel yrel)))

(defgeneric resize (self xrel yrel)
  (:method (self xrel yrel)))

(defgeneric focused (self)
  (:method (self)))

(defgeneric lost-focuse (self)
  (:method (self)))

(defgeneric wheel (self delta)
  (:method (self delta)))

(defgeneric .target (self)
  (:method ((self null)) nil))

(defclass app ()
  ((win :initarg :win :accessor .win)
   (width :initarg :width :initform 800 :accessor .width)
   (height :initarg :height :initform 600 :accessor .height)
   (font :initform nil :accessor .font)
   (views :initarg :views :initform '() :accessor .views)
   (mouse-x :initform 0 :accessor .mouse-x)
   (mouse-y :initform 0 :accessor .mouse-y)
   (selected-module :initform nil :accessor .selected-module)
   (selected-pattern :initform nil :accessor .selected-pattern)
   (focused-view :initform nil :accessor .focused-view)
   (click-target-module :initform (make-array +mouse-button-count+))
   (drag-resize-module :initform nil :accessor .drag-resize-module)
   (dragging :initform nil :accessor .dragging)
   (drag-state :initform nil :accessor .drag-state)
   (connect-from-module :initform nil :accessor .connect-from-module)
   (song-file :initform nil :accessor .song-file)
   (shift-key-p :initform nil :accessor .shift-key-p)
   (mbox :initform (sb-concurrency:make-mailbox) :accessor .mbox)))

(defmethod .modules ((self app))
  (loop for view in (.views self)
        if (typep view 'module)
          collect view))

(defmethod (setf .focused-view) (view (self app))
  (lost-focuse (.focused-view self))
  (focused (setf (slot-value self 'focused-view) view)))

(defmethod (setf .song-file) :after (value (self app))
  (sdl2:set-window-title (.win self) (or value "----")))

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

(defmethod close ((self view) &key abort)
  (declare (ignore abort)))

(defun click-target-module (button)
  (aref (slot-value *app* 'click-target-module) button))

(defun (setf click-target-module) (value button)
  (setf (aref (slot-value *app* 'click-target-module) button) value))

(defun add-view (view)
  (setf (.views *app*) (append(.views *app*) (list view))))

(defun remove-view (module)
  (setf (.views *app*) (remove module (.views *app*))))

(defun view-at-mouse (app)
  (loop for view in (.views app)
          thereis (and (<= (.absolute-x view) (.mouse-x app) (+ (.absolute-x view) (.width view)))
                       (<= (.absolute-y view) (.mouse-y app) (+ (.absolute-y view) (.height view)))
                       view)))

(defmethod child-view-at ((self view) x y)
  (loop for view in (.children self)
          thereis (and (<= (.x view) x (+ (.x view) (.width view)))
                       (<= (.y view) y (+ (.y view) (.height view)))
                       view)))

(defmethod .root-parent ((self view))
  (aif (.parent self)
       (.root-parent it)
       self))

(defmethod .parent-by-class ((self view) class)
  (let ((parent (.parent self)))
    (cond ((null parent)
           nil)
          ((typep parent class)
           parent)
          (t
           (.parent-by-class parent class)))))

(defmethod .max-child-width ((self view))
  (loop for child in (.children self)
        maximize (.width child)))

(defmethod .max-child-height ((self view))
  (loop for child in (.children self)
        maximize (.height child)))

(defmethod .children-bounds ((self view))
  (values
   (loop for child in (.children self)
         minimize (.absolute-x child))
   (loop for child in (.children self)
         minimize (.absolute-y child))
   (loop for child in (.children self)
         maximize (+ (.absolute-x child) (.width child)))
   (loop for child in (.children self)
         maximize (+ (.absolute-y child) (.height child)))))

(defmethod mousebuttondown ((self view) button state clicks x y)
  (call-next-method)
  (awhen (child-view-at self x y)
    (mousebuttondown it button state clicks
                     (translate-child-x self it x)
                     (translate-child-y self it y))))

(defmethod mousebuttonup ((self view) button state clicks x y)
  (call-next-method)
  (awhen (child-view-at self x y)
    (mousebuttonup it button state clicks
                   (translate-child-x self it x)
                   (translate-child-y self it y))))

(defmethod click ((self view) button x y)
  (call-next-method)
  (awhen (child-view-at self x y)
    (click it button
           (translate-child-x self it x)
           (translate-child-y self it y))))

(defmethod drop ((self view) dropped x y button)
  (call-next-method)
  (awhen (child-view-at self x y)
    (drop it dropped
          (translate-child-x self it x)
          (translate-child-y self it y)
          button)))

(defmethod mousemotion ((self view) x y xrel yrel state)
  (call-next-method)
  (awhen (child-view-at self x y)
    (mousemotion it
                 (translate-child-x self it x)
                 (translate-child-y self it y)
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

(defmethod translate-child-x ((self view) (child view) x)
  (- x (.x child)))

(defmethod translate-child-y ((self view) (child view) y)
  (- y (.y child)))

(defmethod render ((self view) renderer)
  (unless (typep self 'text)
    (apply #'sdl2:set-render-draw-color renderer (.color self))
    (sdl2:render-draw-rect renderer
                           (sdl2:make-rect (.absolute-x self)
                                           (.absolute-y self)
                                           (.width self)
                                           (.height self))))
  (loop for child in (.children self)
        do (render child renderer))
  (call-next-method))

(defmethod render-connection ((self view) r)
  (loop for child in (.children self)
        do (render-connection child r))
  (call-next-method))

(defmethod resize :after ((self view) xrel yrel)
  (resized self))

(defmethod resized ((self view))
  (loop for child in (.children self)
        do (resized child))
  (call-next-method))

(defmethod wheel ((self view) delta)
  (call-next-method)
  (awhen (child-view-at self
                        (- (.mouse-x *app*) (.absolute-x self))
                        (- (.mouse-y *app*) (.absolute-y self)))
    (wheel it delta)))

(defclass module (drag-resize-mixin
                  drag-move-mixin
                  drag-connect-mixin
                  view)
  ((model :initarg :model :accessor .model)))

(defmethod initialize-instance :after ((self module) &key)
  (initialize self))

(defmethod initialize ((self module))
  (unless (typep self 'sequencer-module) ;きれいじゃない
    (add-child self
               (make-instance 'text
                              :reader (lambda () (.name (.model self)))
                              :writer (lambda (value) (setf (.name (.model self)) value))
                              :x *layout-space*
                              :y *layout-space*))))

(defmethod close ((self module) &key abort)
  (close (.model self) :abort abort))

(defmethod keydown ((self module) value scancode mod-value)
  (if (and (sdl2:scancode= scancode :scancode-delete)
           (not (zerop (logand mod-value sdl2-ffi:+kmod-shift+)))
           (not (zerop (logand mod-value sdl2-ffi:+kmod-ctrl+)))
           (not (eq self (.sequencer *audio*)))
           (not (eq self (.master *audio*))))
      (progn
        (disconnect-all self)
        (remove-view self)
        (close self))
      (call-next-method)))

(defmethod mousebuttondown :before ((self null) button state clicks x y)
  (setf (.selected-module *app*) nil))

(defmethod mousebuttondown :before ((self module) button state clicks x y)
  (setf (.selected-module *app*) self)
  (setf (.views *app*)
        (stable-sort (.views *app*) (lambda (x y)
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
                (drop (view-at-mouse *app*) self
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

(defmethod drag-start ((self drag-connect-mixin) x y
                       (button (eql sdl2-ffi:+sdl-button-right+)))
  (setf (.connect-from-module *app*) self)
  (call-next-method))

(defmethod drop ((self drag-connect-mixin) (dropped drag-connect-mixin) x y
                 (button (eql sdl2-ffi:+sdl-button-right+)))
  (let ((from (.connect-from-module *app*)))
    (when (and from (not (eq from self)))
      (if (member (.model self) (.out from))
          (disconnect from self)
          (connect from self)))
    (setf (.connect-from-module *app*) nil))
  (call-next-method))

(defun compute-connection-points (from to)
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
    (let* ((offset-x (if (typep from 'track-view)
                         (.offset-x (.parent from))
                         0))
           (offset-y (if (typep from 'track-view)
                         (.offset-y (.parent from))
                         0))
           (x1 (- (.absolute-center-x from) offset-x))
           (y1 (- (.absolute-center-y from) offset-y))
           (x2 (.absolute-center-x to))
           (y2 (.absolute-center-y to))
           (xs1 (- (.absolute-x from) offset-x))
           (ys1 (- (.absolute-y from) offset-y))
           (xs2 (+ xs1 (.width from)))
           (ys2 ys1)
           (xs3 xs2)
           (ys3 (+ ys2 (.height from)))
           (xs4 xs1)
           (ys4 ys3)
           (xe1 (.absolute-x to))
           (ye1 (.absolute-y to))
           (xe2 (+ xe1 (.width to)))
           (ye2 ye1)
           (xe3 xe2)
           (ye3 (+ ye2 (.height to)))
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
          (values xs ys xe ye))))))

(defmethod render-connection ((self drag-connect-mixin) r)
  (loop for out-model in (.out self)
        for out = (loop for module in (.modules *app*)
                          thereis (and (eq out-model (.model module))
                                       module))
        do (multiple-value-bind (xs ys xe ye) (compute-connection-points self out)
             (let ((original-xs xs)
                   (original-ys ys))
               (when (typep self 'track-view)
                 (let ((partial-view (.parent self)))
                   (setf xs (min (max xs (.absolute-x partial-view))
                                 (+ (.absolute-x partial-view)
                                    (.width partial-view))))
                   (setf ys (min (max ys (.absolute-y partial-view))
                                 (+ (.absolute-y partial-view)
                                    (.height partial-view))))))
               (apply #'sdl2:set-render-draw-color r *connection-line-color*)
               (sdl2:render-draw-line r xs ys xe ye)
               (when (and (= xs original-xs) (= ys original-ys))
                 (apply #'sdl2:set-render-draw-color r *connection-point-color*)
                 (sdl2:render-fill-rect r
                                        (sdl2:make-rect (- xs 2) (- ys 2) 5 5))))))
  (when (eq self (.connect-from-module *app*))
    (apply #'sdl2:set-render-draw-color r *connection-line-color*)
    (sdl2:render-draw-line r
                           (.absolute-center-x self)
                           (.absolute-center-y self)
                           (.mouse-x *app*)
                           (.mouse-y *app*)))
  (call-next-method))

(defclass disable-drag-connect-mixin ()
  ())

(defmethod drag-start ((self disable-drag-connect-mixin) x y (button (eql 3))))

(defmethod drop ((self disable-drag-connect-mixin) dropped x y (button (eql 3))))

(defclass label (function-value-mixin view renderable)
  ((last-value :initform "" :accessor .last-value)
   (last-color :accessor .last-color)
   (texture :initform nil :accessor .texture))
  (:default-initargs :width 0 :height 0 :value "くえっ"))

(defmethod initialize-instance :after ((self label) &key)
  (setf (.last-color self) (.color self)))

(defmethod render ((self label) renderer)
  (let ((value (.value self)))
    (when (not (zerop (length value)))
      (when (or (not (.texture self))
                (not (equal value (.last-value self)))
                (not (equal (.color self) (.last-color self))))
        (setf (.last-value self) value)
        (setf (.last-color self) (.color self))
        (awhen (.texture self)
          (sdl2:destroy-texture it))
        (let ((surface (apply #'sdl2-ttf:render-utf8-solid
                              (.font *app*)
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
  ((label-view :accessor .label-view))
  (:default-initargs :width 50 :height 30))

(defmethod initialize-instance :after ((self button) &key label)
  (let ((label (make-instance 'label :value label :x 5 :y 2)))
    (add-child self label)
    (setf (.label-view self) label)
    (setf (.width self) (+ 10 (.width label))
          (.height self) (+ 4 (.height label)))))

(defmethod .label ((self button))
  (.value (.label-view self)))

(defmethod render :after ((self button) renderer)
  ;; after でやるので初回描画時は崩れてるはずだけど妥協
  (let ((label (.label-view self)))
    (setf (.width self) (+ 10 (.width label))
          (.height self) (+ 4 (.height label)))))

(defclass onchange-mixin ()
  ((onchange :initarg :onchange :initform (constantly nil) :accessor .onchange)))

(defclass text (view renderable)
  ((label :accessor .label)
   (edit-buffer :initform "" :accessor .edit-buffer)
   (cursor-position :initform 0 :accessor .cursor-position)
   (focused :initform nil :accessor .focused)
   (reader :initarg :reader :accessor .reader)
   (writer :initarg :writer :accessor .writer))
  (:default-initargs :height (+ *char-height* 4)
                     :width (+ (* *char-width* 12) 4)))

(defmethod .edit-buffer :around ((self text))
  (if (.focused self)
      (call-next-method)
      (setf (.edit-buffer self) (funcall (.reader self)))))

(defmethod initialize-instance :after ((self text) &key)
  (setf (.label self) (make-instance 'label
                                     :value (lambda () (.edit-buffer self))
                                     :x 3 :y 1))
  (setf (.cursor-position self) (length (.edit-buffer self)))
  (add-child self (.label self)))

(defmethod render ((self text) renderer)
  (let ((cursor-x (+ (.absolute-x self)
                     2
                     (* *char-width* (.cursor-position self))))
        (cursor-y (+ (.absolute-y self) 2))
        (cursor-w *char-width*)
        (cursor-h *char-height*))
    (when (.focused self)
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (sdl2:render-fill-rect
       renderer
       (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))

      (sdl2:set-render-draw-color renderer #xff #xff #x22 #xff)
      (sdl2:render-draw-rect renderer
                             (sdl2:make-rect (.absolute-x self)
                                             (.absolute-y self)
                                             (.width self)
                                             (.height self)))))
  (call-next-method))

(defmethod click ((self text) button x y)
  (unless (eq self (.focused-view *app*))
    (setf (.focused-view *app*) self)))

(defmethod focused ((self text))
  (setf (.focused self) t)
  (setf (.cursor-position self) (length (.edit-buffer self))))

(defmethod lost-focuse ((self text))
  (funcall (.writer self) (.edit-buffer self))
  (setf (.focused self) nil))

(defmethod keydown ((self text) value scancode mod-value)
  (let* ((shift-p (not (zerop (logand mod-value sdl2-ffi:+kmod-shift+))))
         (ctrl-p (not (zerop (logand mod-value sdl2-ffi:+kmod-ctrl+))))
         (cursor-position (.cursor-position self))
         (edit-buffer (.edit-buffer self)))
    (cond ((sdl2:scancode= scancode :scancode-left)
           (when (< 0 cursor-position)
             (decf (.cursor-position self))))
          ((sdl2:scancode= scancode :scancode-right)
           (when (< cursor-position (length edit-buffer))
             (incf (.cursor-position self))))
          ((and (sdl2:scancode= scancode :scancode-backspace)
                ctrl-p)
           (setf (.edit-buffer self)
                 (concatenate 'string (subseq edit-buffer cursor-position)))
           (setf (.cursor-position self) 0))
          ((sdl2:scancode= scancode :scancode-backspace)
           (when (and (< 0 cursor-position)
                      (<= cursor-position (length edit-buffer)))
             (setf (.edit-buffer self)
                   (concatenate 'string
                                (subseq edit-buffer 0 (1- cursor-position))
                                (subseq edit-buffer cursor-position)))
             (decf (.cursor-position self))))
          ((sdl2:scancode= scancode :scancode-delete)
           (when (< cursor-position (length edit-buffer))
             (setf (.edit-buffer self)
                   (concatenate 'string
                                (subseq edit-buffer 0 cursor-position)
                                (subseq edit-buffer (1+ cursor-position))))))
          ((sdl2:scancode= scancode :scancode-return)
           (setf (.focused-view *app*) nil))
          ((sdl2:scancode= scancode :scancode-escape)
           (setf (.edit-buffer self) (funcall (.reader self)))
           (setf (.focused-view *app*) nil))
          ((ignore-errors (graphic-char-p (code-char value)))
           (setf (.edit-buffer self)
                 (concatenate 'string
                              (subseq edit-buffer 0 cursor-position)
                              (string (funcall (if shift-p #'char-upcase #'identity)
                                               (code-char value)))
                              (subseq edit-buffer cursor-position)))
           (incf (.cursor-position self)))
          (t (call-next-method)))))

(defclass slider (onchange-mixin
                  function-value-mixin drag-mixin view renderable)
  ((min :initarg :min :initform 0.0d0 :accessor .min)
   (max :initarg :max :initform 1.0d0 :accessor .max)))

(defmethod initialize-instance :after ((self slider) &key)
  (add-child self (make-instance 'label :value (lambda () (format nil "~,5f" (.value self)))
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

(defclass partial-view (view renderable)
  ((zoom :initarg :zoom :initform 100 :accessor .zoom)
   (offset-x :initarg :offset-x :initform 0 :accessor .offset-x)
   (offset-y :initarg :offset-y :initform 0 :accessor .offset-y))
  (:default-initargs :color (list #xff #x00 #x00 #x80)))

(defmethod child-view-at ((self partial-view) x y)
  (loop for view in (.children self)
        ;; なんで x の方はずらさなくていいの？
          thereis (and (<= (.x view) x (+ (.x view) (.width view)))
                       (<= (.y view) (+ y (.offset-y self)) (+ (.y view) (.height view)))
                       view)))

(defmethod render ((self partial-view) renderer)
  ;; texture を width x height にして .absolute-x/y を変え他方が効率よさそう
  (let* ((texture-width
           (loop for child in (.children self)
                 maximize (+ (.absolute-x child) (.width child))))
         (texture-height
           (max (loop for child in (.children self)
                      maximize (+ (.absolute-y child) (.height child)))
                (+ (.absolute-y self) (.height self))))
         (texture (sdl2:create-texture renderer :rgba8888 :target
                                       texture-width texture-height)))
    (unwind-protect
         (let ()
           (sdl2:set-render-target renderer texture)
           (sdl2:set-texture-blend-mode texture :blend)
           (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #xff)
           (sdl2:render-clear renderer)

           (call-next-method)

           (sdl2:set-render-target renderer nil)
           (let* ((dst-x (.absolute-x self))
                  (dst-y (.absolute-y self))
                  (dst-w (.width self))
                  (dst-h (.height self))
                  (src-x (+ (.absolute-x self) (.offset-x self)))
                  (src-y (+ (.absolute-y self) (.offset-y self)))
                  (src-w dst-w)
                  (src-h dst-h)
                  (dst-rect (sdl2:make-rect dst-x dst-y dst-w dst-h))
                  (src-rect (sdl2:make-rect src-x src-y src-w src-h)))
             (sdl2:render-copy renderer texture :source-rect src-rect :dest-rect dst-rect)))
      (sdl2:destroy-texture texture))))

(defmethod wheel ((self partial-view) delta)
  (multiple-value-bind (x1 y1 x2 y2) (.children-bounds self)
    (declare (ignore x1 y1))
    (if (.shift-key-p *app*)
        (setf (.offset-x self)
              (max 0 (min
                      (- (.offset-x self) (* 5 delta))
                      (- x2 (+ (.absolute-x self) (.width self))))))        
        (setf (.offset-y self)
              (max 0 (min
                      (- (.offset-y self) (* 5 delta))
                      (- y2 (+ (.absolute-y self) (.height self)))))))))

(defmethod translate-child-x ((self partial-view) (child view) x)
  (+ (call-next-method) (.offset-x self)))

(defmethod translate-child-y ((self partial-view) (child view) y)
  (+ (call-next-method) (.offset-y self)))

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
                                             ;; 最後の方にスクロールしたとき表示がひずむので適当に大きく
                                             (* h 3)))
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
             (let* ((value (- cursor-x (case (mod cursor-x +column-width+)
                                         (0 2)
                                         (4 4)
                                         (5 1)
                                         (t 0)))))
               (when (<= 0 value)
                 (setf (.cursor-x self) value))))
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

(defclass pattern-editor-line (label)
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

(defclass track-view (drag-mixin
                      drag-connect-mixin
                      drop-mixin
                      view)
  ((model :initarg :model :accessor .model
          :initform (make-instance 'track
                                   :width 690 :height *track-height*))))

(defmethod initialize-instance :after ((self track-view) &key)
  (loop for pattern-position in (.pattern-positions (.model self))
        do (add-pattern-after self pattern-position)))

(defmethod mousebuttondown ((self track-view)
                            (button (eql sdl2-ffi:+sdl-button-right+))
                            state clicks x y)
  (setf (.drag-state *app*)
        (make-instance 'drag-state :target self :button button
                                   :x x :y y :state state)))

(defmethod resized ((self track-view))
  (let* ((parent (.parent self))
         (sequencer-module (.parent-by-class self 'sequencer-module))
        (index (position self (.track-views sequencer-module))))
    (setf (.x self) 0)
    (setf (.y self) (* *track-height* index))
    (setf (.height self) *track-height*)
    (setf (.width self) (max (.width parent) (* (+ 16 (.end (.model sequencer-module)))
                                                *pixcel-per-line*))))
  (call-next-method))

(defmethod drop ((self track-view) (dropped drag-connect-mixin) x y (button (eql 3))))

(defun pixcel-to-line (pixcel)
  (* (round (/ pixcel *pixcel-per-line*) 4) 4))

(defun line-to-pixcel (line)
  (* *pixcel-per-line* line))

(defmethod click ((self track-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (let ((module (.selected-pattern *app*)))
    (if (typep module 'pattern-module)
        (let* ((start (pixcel-to-line x))
               (end (+ start (.length (.model module)))))
          (when (every (lambda (x)
                         (or (<= end (.start x))
                             (<= (.end x) start)))
                       (.pattern-positions (.model self)))
            (add-pattern self module start end)))
        (call-next-method))))

(defclass pattern-position-view (drag-mixin
                                 name-mixin
                                 view
                                 renderable)
  ((model :initarg :model :accessor .model)
   (move-delta-x :initform 0 :accessor .move-delta-x)))

(defmethod initialize-instance :after ((self pattern-position-view) &key)
  (add-child self
             (make-instance 'label
                            :value (lambda () (.name (.model self)))
                            :x *layout-space*
                            :y *layout-space*)))

(defmethod click ((self pattern-position-view)
                  (button (eql sdl2-ffi:+sdl-button-right+))
                  x y)
  (remove-pattern (.parent-by-class self 'track-view) self)
  (call-next-method))

(defmethod drag ((self pattern-position-view) xrel yrel
                 (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((pixcel (+ (.x self) (.move-delta-x self) xrel))
         (line (pixcel-to-line pixcel))
         (rounded-pixcel (line-to-pixcel line)))
    (setf (.x self) rounded-pixcel
          (.move-delta-x self) (- pixcel rounded-pixcel))))

(defmethod drag-end ((self pattern-position) x y
                     (button (eql sdl2-ffi:+sdl-button-left+)))
  (setf (.move-delta-x self) 0)
  (call-next-method))

(defmethod drop ((self track-view) (pattern-position-view pattern-position-view) x y button)
  (let* ((pattern-position (.model pattern-position-view))
         (delta (- (pixcel-to-line (.x pattern-position-view)) (.start pattern-position))))
    (incf (.start pattern-position) delta)
    (incf (.end pattern-position) delta)
    (update-sequencer-end)))

(defclass sequencer-partial-view (partial-view)
  ())

(defmethod resized ((self sequencer-partial-view))
  (let ((sequencer-module (.root-parent self)))
    (setf (.x self) *layout-space*)
    (setf (.y self) 26)
    (setf (.width self) (- (.width sequencer-module) (* *layout-space* 2)))
    (setf (.height self) (- (.height sequencer-module)
                            (.y self)
                            *layout-space*)))
  (call-next-method))

(defclass sequencer-module (module)
  ((track-views :initform nil :accessor .track-views)
   (partial-view :accessor .partial-view))
  (:default-initargs :model (make-instance 'sequencer)))

(defmethod initialize-instance :after ((self sequencer-module) &key)
  (let* ((play-button (make-instance 'button :label "▶" :x 5 :y *layout-space*))
         (add-track-button (make-instance 'button :label "+track" :x 35 :y *layout-space*))
         (bpm (make-instance 'label
                             :value (lambda ()
                                      (format nil "BPM ~f" (.bpm (.model self))))
                             :x 100 :y *layout-space*))
         (partial-view (make-instance 'sequencer-partial-view)))
    (add-child self play-button)
    (add-child self add-track-button)
    (add-child self bpm)
    (add-child self partial-view)
    (setf (.partial-view self) partial-view)
    (loop for track in (.tracks (.model self))
          do (add-new-track-after self track))
    (let ((sequencer self))
      (defmethod click ((self (eql play-button)) (button (eql 1)) x y)
        (if (.playing *audio*)
            (stop)
            (play)))
      (defmethod click ((self (eql add-track-button)) (button (eql 1)) x y)
        (add-new-track sequencer)))
    (resized self)))

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

(defmethod drag-start ((self sequencer-module) x y (button (eql 3)))
  "track-view にディスパッチする。"
  (awhen (child-view-at self x y)
    (drag-start it (- x (.x it)) (- y (.y it)) button)))

(defmethod drop ((self sequencer-module) (dropped drag-connect-mixin) x y (button (eql 3)))
  "track-view にディスパッチする。"
  (awhen (child-view-at self x y)
    (drop it dropped (- x (.x it)) (- y (.y it)) button)))

(defmethod add-new-track ((self sequencer-module))
  (add-new-track-after self (add-new-track (.model self))))

(defmethod add-new-track-after ((self sequencer-module) (track track))
  (let ((track-view (make-instance 'track-view :model track)))
    (add-child (.partial-view self) track-view)
    (setf (.track-views self) (append (.track-views self) (list track-view)))
    (resized track-view)
    track-view))

(defclass pattern-module (module)
  ((pattern-editor :accessor .pattern-editor
                   :initform (make-instance 'pattern-editor)))
  (:default-initargs :model (make-instance 'pattern)))

(defmethod initialize-instance :after ((self pattern-module) &key)
  (let* ((pattern-editor (.pattern-editor self))
         (octave (make-instance 'label
                                :value (lambda ()
                                         (format nil "~d" (.octave pattern-editor)))
                                :x (- (.width self) (* *char-width* 4) *layout-space*)
                                :y *layout-space*))
         (edit-step (make-instance 'label
                                   :value (lambda ()
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

(defmethod add-pattern ((track-view track-view)
                        (pattern-module pattern-module)
                        start end)
  (let ((pattern-position
          (add-pattern (.model track-view)
                       (.model pattern-module) start end)))
    (add-pattern-after track-view pattern-position)))

(defmethod add-pattern-after ((track-view track-view)
                              (pattern-position pattern-position))
  (let ((view (make-instance 'pattern-position-view :model pattern-position)))
    (add-child track-view view)
    (setf (.x view) (* *pixcel-per-line* (.start pattern-position))
          (.y view) 2
          (.width view) (* *pixcel-per-line* (- (.end pattern-position)
                                                (.start pattern-position)))
          (.height view) (- (.height track-view) 4))))

(defmethod close ((self pattern-module) &key abort)
  (declare (ignore abort))
  (loop for track-view in (.track-views *sequencer-module*)
        do (loop for pattern-position-view in (.children track-view)
                 if (and (typep pattern-position-view 'pattern-position-view)
                         (eql (.model self)
                              (.pattern (.model pattern-position-view))))
                   do (remove-pattern track-view pattern-position-view))))

(defmethod mousebuttondown :before ((self pattern-module) button state clicks x y)
  (setf (.selected-pattern *app*) self))

(defmethod remove-pattern ((track-view track-view)
                           (pattern-position-view pattern-position-view))
  (remove-child track-view pattern-position-view)
  (remove-pattern (.model track-view) (.model pattern-position-view)))

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
                     'label :x 20 :y 20
                     :value (lambda () (format nil "~,5f" (.value (.model self)))))))
    (add-child self value-text)))

(defclass sin-osc-module (module osc-module-mixin)
  ()
  (:default-initargs :model (make-instance 'sin-osc)))

(defclass saw-osc-module (module osc-module-mixin)
  ()
  (:default-initargs :model (make-instance 'saw-osc)))

(defclass adsr-module (module)
  ()
  (:default-initargs :model (make-instance 'adsr)))

(defmethod initialize-instance :after ((self adsr-module) &key)
  (let ((x *layout-space*)
        (y (+ *font-size* (* *layout-space* 2)))
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
  (let ((button (make-instance 'button :label "Open" :x *layout-space*
                                       :y (+ *font-size* (* *layout-space* 2)))))
    (add-child self button)
    (defmethod click ((button (eql button)) btn x y)
      (open-editor (.model self)))))

(defmethod close ((self plugin-module) &key abort)
  (close (.model self) :abort abort)
  (call-next-method))

(defclass operand-module (module)
  ())

(defclass volume-controller-mixin ()
  ((volume-slider :initarg :volume-slide :accessor .volume-slider)))

(defmethod initialize-instance :after ((self volume-controller-mixin) &key)
  (add-child self
             (setf (.volume-slider self)
                   (make-instance 'slider
                                  :value (lambda () (.volume (.model self)))
                                  :onchange (lambda (x) (setf (.volume (.model self)) x)))))
  (resized self))

(defmethod resized ((self volume-controller-mixin))
  (let ((slider (.volume-slider self))
        (x *layout-space*)
        (y (+ *font-size* (* *layout-space* 2)))
        (width (- (.width self) (* 2 *layout-space*)))
        (height (+ *font-size* (round (/ *layout-space* 2)))))
    (setf (.x slider) x)
    (setf (.y slider) y)
    (setf (.width slider) width)
    (setf (.height slider) height)
    (call-next-method)))

(defclass gain-module (volume-controller-mixin module)
  ())

(defclass master-module (volume-controller-mixin module)
  ()
  (:default-initargs :model (make-instance 'master)))

(defclass menu-view (view renderable)
  ((filter :initform nil :accessor .filter)
   (buttons :initform nil :accessor .buttons))
  (:default-initargs :width 400 :height 300))

(defmethod initialize-instance :after ((self menu-view) &key)
  (let ((button (make-instance 'button :label "Manage Plugins")))
    (add-child self button)
    (push button (.buttons self))
    (defmethod click ((button (eql button)) btn x y)
      (sb-ext:run-program *plugin-host-exe* nil :wait nil)
      (close self)))
  (loop for (name class . initargs)
          in `(("Pattern" pattern)
               ("Sin" sin-osc)
               ("Saw" saw-osc)
               ("Adsr" adsr)
               ("Op Add" op-add)
               ("Op Multi" op-multi)
               ("Gain" gain))
        do (let ((button (make-instance 'menu-builtin-button
                                        :label name
                                        :class class
                                        :initargs initargs)))
             (add-child self button)
             (push button (.buttons self))))
  (loop for plugin-description in (load-known-plugins)
        do (let ((button (make-instance 'menu-plugin-button
                                        :label (.name plugin-description)
                                        :plugin-description plugin-description)))
             (add-child self button)
             (push button (.buttons self))))
  (setf (.buttons self) (sort (.buttons self) #'string<
                              :key (lambda (x) (string-downcase (.label x))))))

(defmethod render :before ((self menu-view) renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 #xff)
  (sdl2:render-fill-rect renderer (sdl2:make-rect (.absolute-x self)
                                                  (.absolute-y self)
                                                  (.width self)
                                                  (.height self))))

;; text 幅が renderer がないとわからないためのハック
(defmethod render :after ((self menu-view) renderer)
  (when (null (.filter self))
    (setf (.filter self) "")))

(defmethod (setf .filter) :around (value (self menu-view))
  (when (not (equal (.filter self) value))
    (call-next-method)
    (let ((regex (ppcre:create-scanner
                  (format nil ".*~{~c~^.*~}" (coerce value 'list))
                  :case-insensitive-mode t)))
      (loop for button in (.buttons self)
            with i = 0
            with x = *layout-space*
            with y = *layout-space*
            if (ppcre:scan regex (.label button))
              do (when (< (.width self) (+ x (.width button) *layout-space*))
                   (setf x *layout-space*)
                   (incf y (+ (.height button) *layout-space*)))
                 (setf (.x button) x)
                 (setf (.y button) y)
                 (incf x (+ (.width button) *layout-space*))
                 (if (< (.height self) (+ y (.height button)))
                     (remove-child self button)    
                     (add-child self button))
            else
              do (remove-child self button)))))

(defmethod keydown ((self menu-view) value scancode mod-value)
  (cond ((sdl2:scancode= scancode :scancode-escape)
         (close self))
        ((= value #x08)
         (setf (.filter self)
               (subseq (.filter self) 0 (max 0 (1- (length (.filter self)))))))
        ((<= value 127)
         (setf (.filter self) (format nil "~a~a" (.filter self) (code-char value))))))

(defmethod close ((self menu-view) &key abort)
  (declare (ignore abort))
  (when (eq self (.selected-module *app*))
    (setf (.selected-module *app*) nil))
  (remove-view self)
  (call-next-method))

(defclass menu-builtin-button (button)
  ((class :initarg :class :accessor .class)
   (initargs :initarg :initargs :initform nil :accessor .initargs)))

(defmethod click ((self menu-builtin-button) (button (eql 1)) x y)
  (add-view (make-module
             (apply #'make-instance (.class self)
                    :name (.label self)
                    :x (- (.mouse-x *app*) 10)
                    :y (- (.mouse-y *app*) 10)
                    (.initargs self))))
  (close (.root-parent self)))

(defclass menu-plugin-button (button)
  ((plugin-description :initarg :plugin-description
                       :accessor .plugin-description)))

(defmethod click ((self menu-plugin-button) (button (eql 1)) x y)
  (let* ((plugin-description (.plugin-description self))
         (class (if (.is-instrument plugin-description)
                    'instrument-plugin-model
                    'effect-plugin-model)))
    (add-view (make-module 
               (make-instance class
                              :name (.name plugin-description)
                              :x (- (.mouse-x *app*) 10)
                              :y (- (.mouse-y *app*) 10)
                              :plugin-description plugin-description)))
    (close (.root-parent self))))

(defun open-menu ()
  (let ((module (make-instance 'menu-view
                               :x (- (.mouse-x *app*) 10)
                               :y (- (.mouse-y *app*) 10))))
    (add-view module)
    (setf (.selected-module *app*) module)))

(defmethod make-module :around ((self model))
  (let ((module (call-next-method)))
    (initialize module)
    module))

(defmethod make-module ((self sequencer))
  (make-instance 'sequencer-module :model self))

(defmethod make-module ((self master))
  (make-instance 'master-module :model self))

(defmethod make-module ((self instrument-plugin-model))
  (make-instance 'instrument-plugin-module :model self))

(defmethod make-module ((self effect-plugin-model))
  (make-instance 'effect-plugin-module :model self))

(defmethod make-module ((self pattern))
  (make-instance 'pattern-module :model self))

(defmethod make-module ((self saw-osc))
  (make-instance 'saw-osc-module :model self))

(defmethod make-module ((self sin-osc))
  (make-instance 'sin-osc-module :model self))

(defmethod make-module ((self adsr))
  (make-instance 'adsr-module :model self))

(defmethod make-module ((self operand))
  (make-instance 'operand-module :model self))

(defmethod make-module ((self gain))
  (make-instance 'gain-module :model self))
