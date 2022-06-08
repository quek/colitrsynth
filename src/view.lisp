(in-package :colitrsynth)

;;;; 処理の都合上必要なこ
(defvar *pattern-scroll-lock* nil)
(defparameter *track-height* 30)        ;TODO 固定長で妥協

(defconstant +mouse-button-count+ 16)
(defconstant +column-width+ 7)

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
      (let* ((root (.root-parent self))
             (x (- (.mouse-x *app*) (.absolute-x root)))
             (y (- (.mouse-y *app*) (.absolute-y root))))
        (case clicks
          (2 (double-click root button x y))
          (t (click root button x y)))))))

(defgeneric click (self button x y)
  (:method (self button x y)))

(defgeneric double-click (self button x y)
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
          ((sdl2:scancode= scancode :scancode-space)
           (if (.playing *audio*)
               (stop)
               (play-with-key)))
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
                  (unless (alexandria:ends-with-subseq ".lisp" file)
                    (setf file (format nil "~a.lisp" file)))
                  (sb-concurrency:send-message
                   (.mbox *app*)
                   (lambda () (save-song file))))))
            :arguments (list *app*)))
          ((and (sdl2:scancode= scancode :scancode-c)
                (.ctrl-key-p *app*))
           (awhen (.selected-module *app*)
             (sdl2-ffi.functions:sdl-set-clipboard-text
              (with-standard-io-syntax
                (let ((*package* (find-package :colitrsynth))
                      (*serialize-table* nil))
                  (with-output-to-string (out)
                    (write (serialize it) :stream out)))))))
          ((and (sdl2:scancode= scancode :scancode-v)
                (.ctrl-key-p *app*))
           (awhen (deserialize (sdl2-ffi.functions:sdl-get-clipboard-text))
             (multiple-value-bind (x y) (sdl2:mouse-state)
               (setf (.x it) x)
               (setf (.y it) y))
             (add-view it))))))

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

(defvar *serialize-table* nil)
(defvar *serialize-refs* nil)

(defun ref-id (object)
  (if *serialize-table*
      (sif (gethash object *serialize-table*)
           it
           (setf it (hash-table-count *serialize-table*)))
      nil))

(defun r (key)
  (gethash key *serialize-table*))

(defmacro s (&body body)
  `(progn
     (push (lambda () ,@body) *serialize-refs*)
     nil))

(defgeneric serialize (self)
  (:method (x)
    x)
  (:method ((self cons))
    `(list
      ,@(loop for x in self
              collect (serialize x))))
  (:method ((self array))
    (if (loop for x across self
              always (numberp x))
        `(coerce #(,@(coerce self 'list)) ',(type-of self))
        `(make-array ,(length self) :element-type ',(array-element-type self)
                                    :initial-contents ,(serialize (coerce self 'list)))))
  (:method :after ((self standard-object))
    (ref-id self)))

(defgeneric serialize-ref (self &key))

(defmethod serialize-ref :around (self &key)
  (if *serialize-table*
      (call-next-method)
      nil))

(defmethod serialize-ref ((self null) &key)
  nil)

(defmethod serialize-ref ((self cons) &key)
  `(let ((y (make-list ,(length self))))
     ,@(loop for module in self
             for i from 0
             collect `(s (setf (nth ,i y) (r ,(ref-id module)))))
     y))

(defmethod serialize-ref ((self standard-object) &key accessor)
  `(s (setf (,accessor x) (r ,(ref-id self)))))


(defgeneric deserialize (in)
  (:method ((in string))
    (with-input-from-string (in in)
      (deserialize in)))
  (:method ((in stream))
    (ignore-errors
     (with-standard-io-syntax
       (let ((*package* (find-package :colitrsynth)))
         (eval (read in)))))))

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
   (ctrl-key-p :initform nil :accessor .ctrl-key-p)
   (mbox :initform (sb-concurrency:make-mailbox) :accessor .mbox)))

(defun ctrl-key-p ()
  (.ctrl-key-p *app*))

(defun shift-key-p ()
  (.shift-key-p *app*))

(defun play-with-key ()
  (cond ((ctrl-key-p)
         (play-from-start))
        ((shift-key-p)
         (play-from-last))
        (t
         (play-from-current))))

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
  ((color :initarg :color :initform *default-color* :accessor .color)
   (x :initarg :x :initform 0 :accessor .x)
   (y :initarg :y :initform 0 :accessor .y)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)
   (parent :initarg :parent :initform nil :accessor .parent)
   (children :initarg :children :initform nil :accessor .children)))

(defmethod close ((self view) &key abort)
  (declare (ignore abort)))

(defun click-target-module (button)
  (aref (slot-value *app* 'click-target-module) button))

(defun (setf click-target-module) (value button)
  (setf (aref (slot-value *app* 'click-target-module) button) value))

(defun add-view (view)
  (push view (.views *app*)))

(defun addend-view (view)
  (setf (.views *app*)
        (append (.views *app*) (list view))))

(defun remove-view (module)
  (setf (.views *app*) (remove module (.views *app*))))

(defun view-at-mouse (app)
  (loop for view in (reverse (.views app))
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

(defmethod double-click ((self view) button x y)
  (call-next-method)
  (awhen (child-view-at self x y)
    (double-click it button
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

(defmethod serialize :around ((self view))
  `(let ((x (make-instance ',(class-name (class-of self)))))
     ,@(call-next-method)
     x))

(defmethod serialize ((self view))
  `((setf (.x x) ,(.x self)
          (.y x) ,(.y self)
          (.width x) ,(.width self)
          (.height x) ,(.height self)
          (.color x) ,(serialize (.color self)))))

(defun local-mouse-position (view)
  (labels ((f (self x y)
             (let ((parent (.parent self)))
               (if parent
                   (multiple-value-bind (x y) (f parent x y)
                     (values (translate-child-x parent self x)
                             (translate-child-x parent self y)))
                   (values (- x (.absolute-x self))
                           (- y (.absolute-y self)))))))
    (multiple-value-bind (x y) (sdl2:mouse-state)
      (f view x y))))

(defclass render-border-mixin () ())

(defmethod render ((self render-border-mixin) renderer)
  (apply #'sdl2:set-render-draw-color renderer (.color self))
  (sdl2:render-draw-rect renderer
                         (sdl2:make-rect (.absolute-x self)
                                         (.absolute-y self)
                                         (.width self)
                                         (.height self)))
  (call-next-method))

(defclass module (drag-resize-mixin
                  drag-move-mixin
                  drag-connect-mixin
                  render-border-mixin
                  view)
  ())

(defmethod initialize-instance :after ((self module) &key)
  (initialize self))

(defmethod initialize ((self module))
  (unless (typep self 'sequencer-module) ;きれいじゃない
    (add-child self
               (make-instance 'text
                              :reader (lambda () (.name self))
                              :writer (lambda (value) (setf (.name self) value))
                              :x *layout-space*
                              :y *layout-space*))))

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
                                      (not (eql x self))))))

(defmethod render :before ((self module) renderer)
  "選択中のモジュールを見やすくする"
  (when (eq self (.selected-module *app*))
    (let ((texture (sdl2:create-texture renderer :rgba8888 :target
                                        (.width self) (.height self))))
      (sdl2:set-render-target renderer texture)
      (sdl2:set-texture-blend-mode texture :blend)
      (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #xcc) 
      (sdl2:render-fill-rect
       renderer
       (sdl2:make-rect 0 0 (.width self) (.height self)))
      (sdl2:set-render-target renderer nil)
      (let ((dest-rect (sdl2:make-rect (.absolute-x self)
                                       (.absolute-y self)
                                       (.width self)
                                       (.height self))))
        (sdl2:render-copy renderer texture :source-rect nil :dest-rect dest-rect))
      (sdl2:destroy-texture texture))))

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

(defmethod serialize ((self model))
  `((setf (.name x) ,(.name self)
           (.in x) ,(serialize-ref (.in self))
           (.out x) ,(serialize-ref (.out self)))
    ,@(call-next-method)))

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
            (multiple-value-bind (x y) (local-mouse-position self)
              (drag-start self x y (.button drag-state))))
          (drag self xrel yrel (.button drag-state)))
        (call-next-method))))

(defmethod mousebuttonup ((self drag-mixin) button state clicks x y)
  (let ((drag-state (.drag-state *app*)))
    (if (eq self (.target drag-state))
        (progn
          (if (.dragging drag-state)
              (progn
                (multiple-value-bind (x y) (local-mouse-position self)
                  (drag-end self x y button))
                (awhen (view-at-mouse *app*)
                  (multiple-value-bind (x y) (local-mouse-position it)
                    (drop it self x y button))))
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

(defmethod disconnect ((in drag-connect-mixin) (out drag-connect-mixin))
  (disconnect in out))

(defmethod disconnect-all ((self module))
  (disconnect-all self))

(defmethod drag-start ((self drag-connect-mixin) x y
                       (button (eql sdl2-ffi:+sdl-button-right+)))
  (setf (.connect-from-module *app*) self)
  (call-next-method))

(defmethod drop ((self drag-connect-mixin) (dropped drag-connect-mixin) x y
                 (button (eql sdl2-ffi:+sdl-button-right+)))
  (when (not (eq dropped self))
    (if (member self (.out dropped))
        (disconnect dropped self)
        (connect dropped self)))
  (setf (.connect-from-module *app*) nil) ;TODO .connect-from-module って必要？
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
                          thereis (and (eq out-model module)
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
    (let ((xs (.absolute-center-x self))
          (ys (.absolute-center-y self))
          (xe (.mouse-x *app*))
          (ye (.mouse-y *app*)))
      (when (typep self 'track-view)
        (let ((partial-view (.parent self)))
          (setf xs (floor (/ (+ (.absolute-x partial-view)
                                (.width partial-view))
                             2)))
          (setf ys (min (max (- ys (.offset-y partial-view))
                             (.absolute-y partial-view))
                        (+ (.absolute-y partial-view)
                           (.height partial-view))))))
      (sdl2:render-draw-line r xs ys xe ye)))
  (call-next-method))

(defclass disable-drag-connect-mixin ()
  ())

(defmethod drag-start ((self disable-drag-connect-mixin) x y (button (eql 3))))

(defmethod drop ((self disable-drag-connect-mixin) dropped x y (button (eql 3))))

(defclass label (function-value-mixin view)
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

(defclass button (render-border-mixin view)
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

(defclass focus-mixin ()
  ((focused :initform nil :accessor .focused)))

(defmethod click ((self focus-mixin) button x y)
  (unless (eq self (.focused-view *app*))
    (setf (.focused-view *app*) self))
  (call-next-method))

(defmethod focused ((self focus-mixin))
  (setf (.focused self) t)
  (call-next-method))

(defmethod lost-focuse ((self focus-mixin))
  (setf (.focused self) nil)
  (call-next-method))

(defclass onchange-mixin ()
  ((onchange :initarg :onchange :initform (constantly nil) :accessor .onchange)))

(defclass text (focus-mixin view)
  ((label :accessor .label)
   (edit-buffer :initform "" :accessor .edit-buffer)
   (cursor-position :initform 0 :accessor .cursor-position)
   (reader :initarg :reader :accessor .reader)
   (writer :initarg :writer :accessor .writer))
  (:default-initargs :height (+ *char-height* 4)
                     :width (+ (* *char-width* 12) 4)))

(defmethod initialize-instance :after ((self text) &key)
  (setf (.label self) (make-instance 'label
                                     :value (lambda () (.edit-buffer self))
                                     :x 3 :y 1))
  (setf (.cursor-position self) (length (.edit-buffer self)))
  (add-child self (.label self)))

(defmethod click ((self text) button x y)
  ;; single click ではフォーカスしない
  )

(defmethod .edit-buffer :around ((self text))
  (if (.focused self)
      (call-next-method)
      (setf (.edit-buffer self) (funcall (.reader self)))))

(defmethod double-click ((self text) button x y)
  (unless (eq self (.focused-view *app*))
    (setf (.focused-view *app*) self))
  (call-next-method))

(defmethod render ((self text) renderer)
  (let ((cursor-x (+ (.absolute-x self)
                     2
                     (* *char-width* (.cursor-position self))))
        (cursor-y (+ (.absolute-y self) 2))
        (cursor-w *char-width*)
        (cursor-h *char-height*))
    (when (.focused self)
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (sdl2:render-fill-rect renderer
                             (sdl2:make-rect cursor-x cursor-y
                                             cursor-w cursor-h))
      (apply #'sdl2:set-render-draw-color renderer *focused-color*)
      (sdl2:render-draw-rect renderer
                             (sdl2:make-rect (.absolute-x self)
                                             (.absolute-y self)
                                             (.width self)
                                             (.height self)))))
  (call-next-method))

(defmethod focused ((self text))
  (setf (.cursor-position self) (length (.edit-buffer self)))
  (call-next-method))

(defmethod lost-focuse ((self text))
  (funcall (.writer self) (.edit-buffer self))
  (setf (.focused self) nil)
  (call-next-method))

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
                  function-value-mixin drag-mixin
                  render-border-mixin view)
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

(defclass partial-view (view)
  ((zoom :initarg :zoom :initform 100 :accessor .zoom)
   (offset-x :initarg :offset-x :initform 0 :accessor .offset-x)
   (offset-y :initarg :offset-y :initform 0 :accessor .offset-y)))

(defmethod child-view-at ((self partial-view) x y)
  (loop for view in (.children self)
        ;; なんで x の方はずらさなくていいの？
          thereis (and (<= (.x view) x (+ (.x view) (.width view)))
                       (<= (.y view) (+ y (.offset-y self)) (+ (.y view) (.height view)))
                       view)))

(defmethod render ((self partial-view) renderer)
  ;; texture を width x height にして .absolute-x/y を変えた方が効率よさそう
  (let* ((texture-width (.texture-width self))
         (texture-height (.texture-height self))
         (texture (sdl2:create-texture renderer :rgba8888 :target
                                       texture-width texture-height)))
    (unwind-protect
         (let ()
           (sdl2:set-render-target renderer texture)
           (sdl2:set-texture-blend-mode texture :add)
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

(defmethod .texture-height ((self partial-view))
  (max (loop for child in (.children self)
             maximize (+ (.absolute-y child) (.height child)))
       (+ (.absolute-y self) (.height self))))

(defmethod .texture-width ((self partial-view))
  (max (loop for child in (.children self)
             maximize (+ (.absolute-x child) (.width child)))
       (+ (.absolute-x self) (.width self))))

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

(defclass pattern-editor (focus-mixin partial-view)
  ((pattern :accessor .pattern)
   (lines :initform nil :accessor .lines)
   (cursor-x :initform 0 :accessor .cursor-x)
   (cursor-y :initform 0 :accessor .cursor-y)
   (octave :initform 4 :accessor .octave)
   (edit-step :initform 0 :accessor .edit-step)
   (shifting-p :initform nil :accessor .shifting-p)))

(defmethod (setf .cursor-y) :after (value (self pattern-editor))
  (setf (.offset-y self)
        (round (- (* *char-height* value) (/ (.height self) 2)))))

;; TODO もしかすると resized に移すべき？
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
  (call-next-method)
  (when (.focused self)
    (apply #'sdl2:set-render-draw-color renderer *focused-color*)
    (sdl2:render-draw-rect renderer
                           (sdl2:make-rect (.absolute-x self)
                                           (.absolute-y self)
                                           (.width self)
                                           (.height self)))))

(defmethod keydown ((self pattern-editor) value scancode mod-value)
  (unless (.focused self)
    (return-from keydown 'call-next-method))
  (let* ((shift-p (not (zerop (logand mod-value sdl2-ffi:+kmod-shift+))))
         (ctrl-p (not (zerop (logand mod-value sdl2-ffi:+kmod-ctrl+))))
         (max-cursor-y (1- (length (.lines (.pattern self)))))
         (lines (.lines (.pattern self)))
         (line (aref lines (.cursor-y self)))
         (max-line-length 16)
         (cursor-x (.cursor-x self))
         (column-width 7))
    (labels ((on-note ()
               (zerop (mod cursor-x +column-width+)))
             (set-note (note)
               (when (on-note)
                 (setf (.note (aref (.columns line) (floor (/ cursor-x +column-width+)))) note)
                 (if shift-p
                     (let* ((new-column (1+ (floor cursor-x column-width))))
                       (setf (.shifting-p self) t)
                       (when (< new-column max-line-length)
                         (when (<= (.length line) new-column)
                           (extend-column (.pattern self)))
                         (setf (.cursor-x self) (* new-column column-width))
                         (let ((root (.root-parent self)))
                           (setf (.width root) (max (.width root)
                                                    (* *char-width*
                                                       (+ (* new-column column-width)
                                                          12))))))) ;12は適当
                     (step-next self))))
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
                 (step-next self)))
             (set-velocity-0x (velocity)
               (let ((column (aref (.columns line) (floor (/ cursor-x +column-width+)))))
                 (setf (.velocity column)
                       (+ (logand (.velocity column) #xf0)
                          velocity)))
               (step-next self)))
      (cond ((sdl2:scancode= scancode :scancode-f9)
             (setf (.cursor-y self) 0))
            ((sdl2:scancode= scancode :scancode-f10)
             (setf (.cursor-y self) (floor (* (.length (.pattern self)) 1/4))))
            ((sdl2:scancode= scancode :scancode-f11)
             (setf (.cursor-y self) (floor (* (.length (.pattern self)) 2/4))))
            ((sdl2:scancode= scancode :scancode-f12)
             (setf (.cursor-y self) (floor (* (.length (.pattern self)) 3/4))))
            ((or (sdl2:scancode= scancode :scancode-up)
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
             (extend-column (.pattern self)))
            ((or (sdl2:scancode= scancode :scancode-right)
                 (sdl2:scancode= scancode :scancode-l))
             (let* ((value (+ cursor-x (case (mod cursor-x column-width)
                                         (0 4)
                                         (4 1)
                                         (5 2)
                                         (t 0)))))
               (when (< value (* column-width (.length line)))
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
            (t
             (call-next-method)
             ;; TODO ちょっとわけわからんことになっているので何とかしたいです
             ;; ohandle-sdl2-keydown-event から (.focused-view *app*) で直に来ているから
             'call-next-method)))))

(defmethod keyup ((self pattern-editor) value scancode mod-value)
  (cond ((and (.shifting-p self)
              (or (sdl2:scancode= scancode :scancode-lshift)
                  (sdl2:scancode= scancode :scancode-rshift)))
         (setf (.shifting-p self) nil)
         (step-next self)
         (setf (.cursor-x self) 0))
        (t (call-next-method))))

(defmethod step-next ((self pattern-editor))
  (setf (.cursor-y self) (mod (+ (.cursor-y self) (.edit-step self))
                              (length (.lines (.pattern self))))))

(defmethod .texture-height ((self pattern-editor))
  (ceiling (* (call-next-method) 1.5)))

(defclass pattern-editor-line (label)
  ((line :initarg :line :accessor .line)))

(defmethod render :before ((self pattern-editor-line) renderer)
  (let* ((parent (.parent self))
         (position (position self (.lines parent))))
    (setf (.value self)
          (with-output-to-string (out)
            (format out "~2,'0X" position)
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
                                      c s o (.velocity column))))))))
    ;; play position
    (when (= (.current-line (.pattern parent)) position)
      (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
      (let ((play-x (.absolute-x self))
            (play-y (.absolute-y self))
            (play-w (.width self))
            (play-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect play-x play-y play-w play-h))))
    ;; cursor
    (when (and (.focused parent)
               (= (.cursor-y parent) position))
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (let ((cursor-x (* *char-width* (+ (.cursor-x parent) 5)))
            (cursor-y (.absolute-y self))
            (cursor-w (if (zerop (mod (.cursor-x parent) +column-width+))
                          (* *char-width* 3)
                          *char-width*))
            (cursor-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))))))

(defclass track-view (track
                      drag-mixin
                      drag-connect-mixin
                      drop-mixin
                      render-border-mixin
                      view)
  ()
  (:default-initargs :width 690 :height *track-height*))

(defmethod mousebuttondown ((self track-view)
                            (button (eql sdl2-ffi:+sdl-button-right+))
                            state clicks x y)
  (setf (.drag-state *app*)
        (make-instance 'drag-state :target self :button button
                                   :x x :y y :state state)))

(defmethod resized ((self track-view))
  (let* ((parent (.parent self))
         (sequencer-module (.parent-by-class self 'sequencer-module))
         (timeline (.timeline parent))
        (index (position self (.tracks sequencer-module))))
    (setf (.x self) 0)
    (setf (.y self) (+ (* *track-height* index) (.height timeline)))
    (setf (.height self) *track-height*)
    (setf (.width self) (max (.width parent) (* (+ 16 (.end sequencer-module))
                                                *pixcel-per-line*)))
    (setf (.width timeline) (.width self)))
  (call-next-method))

(defmethod drop ((self track-view) (dropped drag-connect-mixin) x y (button (eql 3))))

(defun pixcel-to-line (pixcel)
  ;; 16 ラインにグリッド
  (* (round (/ pixcel *pixcel-per-line*) 16) 16))

(defun line-to-pixcel (line)
  (* *pixcel-per-line* line))

(defmethod click ((self track-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (let ((module (.selected-pattern *app*)))
    (if (typep module 'pattern-module)
        (let* ((start (pixcel-to-line x))
               (end (+ start (.length module))))
          (when (every (lambda (x)
                         (or (<= end (.start x))
                             (<= (.end x) start)))
                       (.pattern-positions self))
            (add-pattern self module start end)))))
  (call-next-method))

(defmethod serialize ((self track-view))
  `((setf (.pattern-positions x) ,(serialize (.pattern-positions self)))
    (loop for i in (.pattern-positions x)
          do (add-child x i))
    ,@(call-next-method)))

(defclass pattern-position-view (pattern-position
                                 drag-mixin
                                 name-mixin
                                 render-border-mixin
                                 view)
  ((move-delta-x :initform 0 :accessor .move-delta-x)))

(defmethod initialize-instance :after ((self pattern-position-view) &key)
  (add-child self
             (make-instance 'label
                            :value (lambda () (.name self))
                            :x *layout-space*
                            :y *layout-space*)))

(defmethod click ((self pattern-position-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (loop for module in (.modules *app*)
        with pattern = (.pattern self)
        if (and (typep module 'pattern-module)
                (eq module pattern))
          do (setf (.selected-pattern *app*) module)
             (loop-finish)))

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
  (let* ((pattern-position pattern-position-view)
         (delta (- (pixcel-to-line (.x pattern-position-view)) (.start pattern-position))))
    (incf (.start pattern-position) delta)
    (incf (.end pattern-position) delta)
    (update-sequencer-end *sequencer-module*)))

(defmethod resized ((self pattern-position-view))
  (setf (.height self) (- (.height (.parent self)) 4)))

(defmethod serialize ((self pattern-position-view))
  `((setf (.start x) ,(.start self)
          (.end x) ,(.end self)
          (.pattern x) ,(serialize-ref (.pattern self) :accessor '.pattern))
    ,@(call-next-method)))

(defclass sequencer-timeline-view (drag-mixin view)
  ((labels :initform nil :accessor .labels)
   (sequencer :initarg :sequencer :accessor .sequencer)))

(defun x-to-rounded-line (x)
  (* (round (/ x *pixcel-per-line*) 4) 4))

(defmethod click ((self sequencer-timeline-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line x)))
    (setf (play-position-line (.play-position sequencer))
          line
          (play-position-line-frame (.play-position sequencer))
          0)))

(defmethod drag-start ((self sequencer-timeline-view) x y
                       (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line x)))
    (setf (.loop-start-line sequencer) line)
    (setf (.loop-end-line sequencer) line)))

(defmethod drag ((self sequencer-timeline-view) xrel yrel
                 (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line (local-mouse-position self))))
    (setf (.loop-end-line sequencer) line)))

(defmethod drag-end ((self sequencer-timeline-view) x y
                     (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line x)))
    (setf (.loop-end-line sequencer) line)
    (when (< (.loop-end-line sequencer) (.loop-start-line sequencer))
      (rotatef (.loop-end-line sequencer) (.loop-start-line sequencer)))))

(defmethod render ((self sequencer-timeline-view) renderer)
  (let* ((sequencer (.sequencer self))
         (end-line (.end sequencer))
         (loop-start (.loop-start-line sequencer))
         (loop-end (.loop-end-line sequencer)))
    (when (/= loop-start loop-end)
      (apply #'sdl2:set-render-draw-color renderer *loop-color*)
      (sdl2:render-fill-rect renderer
                             (sdl2:make-rect
                              (+ (* *pixcel-per-line* loop-start)
                                 (.absolute-x self))
                              (.absolute-y self)
                              (+ (* *pixcel-per-line* (- loop-end loop-start)))
                              (.height self))))
    (apply #'sdl2:set-render-draw-color renderer *default-color*)
    (loop for i from 0 to (/ (+ end-line 16) 4)
          for x = (+ (* *pixcel-per-line* i 4 4 4) (.absolute-x self))
          do (sdl2:render-draw-line renderer
                                    x
                                    (+ (.absolute-y self) (floor (/ (.height self) 2)))
                                    x
                                    (+ (.absolute-y self) (.height self))))
    (call-next-method)))

(defmethod resized ((self sequencer-timeline-view))
  (setf (.height self) 15)     ;width track-view の resized で設定する
  (let ((end-line (max (.end (.parent-by-class self 'sequencer-module))
                       (/ (.width self) *pixcel-per-line*))))
    (loop for i from (length (.labels self)) to (/ end-line 4)
          for label = (make-instance 'label :value (princ-to-string (1+ i))
                                            :x (+ (* *pixcel-per-line* i 4 4 4) 3))
          do (add-child self label)
             (push label (.labels self))))
  (call-next-method))

(defclass sequencer-partial-view (partial-view)
  ((timeline :initarg :timeline :accessor .timeline)))

(defmethod initialize-instance :after ((self sequencer-partial-view) &key)
  (add-child self (.timeline self)))

(defmethod render :after ((self sequencer-partial-view) renderer)
  (let* ((sequencer (.root-parent self))
         (x (max (.absolute-x self)
                 (min
                  (+ (.absolute-x self)
                     (.width self))
                  (+ (.absolute-x self)
                     (* (play-position-line (.play-position sequencer))
                        *pixcel-per-line*)
                     (- (.offset-x self))))))
         (y (.absolute-y self)))
    (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
    (sdl2:render-draw-line renderer x y x (+ y (.height self)))))

(defmethod resized ((self sequencer-partial-view))
  (let ((sequencer-module (.root-parent self)))
    (setf (.x self) *layout-space*)
    (setf (.y self) 26)
    (setf (.width self) (- (.width sequencer-module) (* *layout-space* 2)))
    (setf (.height self) (- (.height sequencer-module)
                            (.y self)
                            *layout-space*)))
  (call-next-method))

(defclass loop-button (button)
  ((fill-color :accessor .fill-color)
   (sequencer :initarg :sequencer :accessor .sequencer))
  (:default-initargs :label "L"))

(defmethod initialize-instance :after ((self loop-button) &key)
  (setf (.fill-color self)
        (if (.looping (.sequencer self))
            *loop-color*
            *background-color*)))
  
(defmethod click ((self loop-button) (button (eql 1)) x y)
  (setf (.fill-color self)
        (if (setf (.looping (.sequencer self))
                  (not (.looping (.sequencer self))))
            *loop-color*
            *background-color*)))

(defmethod render ((self loop-button) render)
  (apply #'sdl2:set-render-draw-color render (.fill-color self))
  (sdl2:render-fill-rect render
                         (sdl2:make-rect (.absolute-x self)
                                         (.absolute-y self)
                                         (.width self)
                                         (.height self)))
  (call-next-method))

(defclass sequencer-module (sequencer module)
  ((partial-view :accessor .partial-view))
  (:default-initargs :color (list #x00 #xff #xff *transparency*)
                     :x 5 :y 5 :width 700 :height 200))

(defmethod initialize-instance :after ((self sequencer-module) &key)
  (let* ((play-button (make-instance 'button :label "▶" :x 5 :y *layout-space*))
         (loop-button (make-instance 'loop-button :x 30 :y *layout-space*
                                     :sequencer self))
         (add-track-button (make-instance 'button :label "+track" :x 55 :y *layout-space*))
         (bpm (make-instance 'label
                             :value (lambda ()
                                      (format nil "BPM ~f" (.bpm self)))
                             :x 115 :y *layout-space*))
         (partial-view (make-instance 'sequencer-partial-view
                                      :timeline  (make-instance 'sequencer-timeline-view
                                                                :sequencer self))))
    (add-child self play-button)
    (add-child self loop-button)
    (add-child self add-track-button)
    (add-child self bpm)
    (add-child self partial-view)
    (setf (.partial-view self) partial-view)
    (loop for track in (.tracks self)
          do (add-new-track-after self track))
    (defmethod click ((play-button (eql play-button)) (button (eql 1)) x y)
      (if (.playing *audio*)
          (stop)
          (play-with-key)))
    (defmethod click ((add-track-button (eql add-track-button)) (button (eql 1)) x y)
      (add-new-track self))
    (resized self)))

(defmethod keydown ((self sequencer-module) value scancode mod-value)
  (cond ((sdl2:scancode= scancode :scancode-1)
         (decf (.bpm self)))
        ((sdl2:scancode= scancode :scancode-2)
         (incf (.bpm self)))
        (t (call-next-method))))

(defmethod drag-start ((self sequencer-module) x y (button (eql 3)))
  "track-view にディスパッチする。"
  (awhen (child-view-at self x y)
    (drag-start it (- x (.x it)) (- y (.y it)) button)))

(defmethod drop ((self sequencer-module) (dropped drag-connect-mixin) x y (button (eql 3)))
  "track-view にディスパッチする。"
  (awhen (child-view-at self x y)
    (drop it dropped (- x (.x it)) (- y (.y it)) button)))

(defmethod add-new-track ((self sequencer-module))
  (let ((track (make-instance 'track-view)))
    (setf (.tracks self)
          (append (.tracks self) (list track)))
    (add-new-track-after self track)
    (resized self)
    track))

(defmethod add-new-track-after ((self sequencer-module) (track-view track-view))
  (add-child (.partial-view self) track-view)
  track-view)

(defmethod serialize ((self sequencer-module))
  `((setf (.tracks x) ,(serialize (.tracks self))
          (.bpm x) ,(.bpm self)
          (.lpb x) ,(.lpb self)
          (.looping x) ,(.looping self))
    ,@(call-next-method)))

(defmethod (setf .tracks) :after (tracks (self sequencer-module))
  (loop for track in tracks
        do (add-new-track-after self track))
  (update-sequencer-end self))

(defclass pattern-module (pattern module)
  ((pattern-editor :accessor .pattern-editor
                   :initform (make-instance 'pattern-editor))))

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
    (setf (.pattern pattern-editor) self
          (.x pattern-editor) *layout-space*
          (.y pattern-editor) (+ *font-size* (* *layout-space* 2))
          (.width pattern-editor) (- (.width self) 10)
          (.height pattern-editor) (- (.height self) (+ 10 *font-size*)))))

(defmethod add-pattern ((track-view track-view) (pattern-module pattern-module) start end)
  (let ((pattern-position-view (make-instance 'pattern-position-view
                                              :pattern pattern-module
                                              :start start :end end)))
    (push pattern-position-view
          (.pattern-positions track-view))
    (update-sequencer-end *sequencer-module*)
    (add-child track-view pattern-position-view)
    (setf (.x pattern-position-view) (* *pixcel-per-line* (.start pattern-position-view))
          (.y pattern-position-view) 2
          (.width pattern-position-view) (* *pixcel-per-line* (- (.end pattern-position-view)
                                                                 (.start pattern-position-view)))
          (.height pattern-position-view) (- (.height track-view) 4))
    pattern-position-view))

(defmethod close ((self pattern-module) &key abort)
  (declare (ignore abort))
  (loop for track-view in (.tracks *sequencer-module*)
        do (loop for pattern-position-view in (.children track-view)
                 if (and (typep pattern-position-view 'pattern-position-view)
                         (eql self
                              (.pattern pattern-position-view)))
                   do (remove-pattern track-view pattern-position-view))))

(defmethod mousebuttondown :before ((self pattern-module) button state clicks x y)
  (setf (.selected-pattern *app*) self))

(defmethod remove-pattern ((track-view track-view)
                           (pattern-position-view pattern-position-view))
  (setf (.pattern-positions track-view)
        (remove pattern-position-view (.pattern-positions track-view)))
  (update-sequencer-end *sequencer-module*)
  (remove-child track-view pattern-position-view))

(defmethod keydown ((self pattern-module) value scancode mod-value)
  ;; TODO pattern-editor にフォーカスしている場合の pattern-editor の keydown をコールする
  (when (eq 'call-next-method
            (keydown (.pattern-editor self) value scancode mod-value))
    (call-next-method)))

(defmethod (setf .width) :after (value (self pattern-module))
  (setf (.width (.pattern-editor self)) (- (.width self) 10)))

(defmethod (setf .height) :after (value (self pattern-module))
  (setf (.height (.pattern-editor self)) (- (.height self) (+ 10 *font-size*))))

(defmethod serialize ((self pattern-module))
  `((setf (.length x) ,(.length self)
          (.lines x) ,(serialize (.lines self))
          (.current-line x) 0)
    (setf (.octave (.pattern-editor x)) ,(.octave (.pattern-editor self))
          (.edit-step (.pattern-editor x)) ,(.edit-step (.pattern-editor self)))
    ,@(call-next-method)))

(defmethod serialize ((self line))
  `(make-instance 'line
                  :columns ,(serialize (.columns self))
                  :lenght ,(.length self)))

(defmethod serialize ((self column))
  `(make-instance 'column
                  :note ,(.note self)
                  :velocity ,(.velocity self)))

(defclass osc-module-mixin ()
  ()
  (:default-initargs :height 50))

(defmethod initialize-instance :after ((self osc-module-mixin) &key)
  (let ((value-text (make-instance 'label
                                   :x 25 :y 25
                                   :value (let ((f (interval-upadate-value
                                                    (lambda () (.value self))
                                                    0.3)))
                                            (lambda ()
                                              (format nil "~,5f" (funcall f)))))))
    (add-child self value-text)))

(defclass sin-osc-module (sin-osc module osc-module-mixin)
  ()
  (:default-initargs :name "Sin"))

(defclass saw-osc-module (saw-osc module osc-module-mixin)
  ()
  (:default-initargs :name "Saw"))

(defclass adsr-module (adsr module)
  ()
  (:default-initargs :name "Adsr" :height 100))

(defmethod initialize-instance :after ((self adsr-module) &key)
  (let ((x *layout-space*)
        (y (+ *font-size* (* *layout-space* 2)))
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

(defmethod serialize ((self adsr-module))
  `((setf (.a x) ,(.a self)
          (.d x) ,(.d self)
          (.s x) ,(.s self)
          (.r x) ,(.r self))
    ,@(call-next-method)))

(defclass plugin-module (module)
  ()
  (:default-initargs :height 48))

(defmethod serialize ((self plugin-module))
  (let ((pd (.plugin-description self)))
    (get-plugin-state self)
    `((setf (.plugin-description x)
            (make-instance 'plugin-description
                           :name ,(.name pd)
                           :format ,(.format pd)
                           :category ,(.category pd)
                           :manufacturer ,(.manufacturer pd)
                           :version ,(.version pd)
                           :file ,(.file pd)
                           :unique-id ,(.unique-id pd)
                           :is-instrument ,(.is-instrument pd)
                           :num-inputs ,(.num-inputs pd)
                           :num-outputs ,(.num-outputs pd)
                           :uid ,(.uid pd))
            (.plugin-state x) ,(serialize (.plugin-state self)))
      ,@(call-next-method)
      (run-plugin-host x)
      (set-plugin-state x))))

(defclass instrument-plugin-module (instrument-plugin-model plugin-module)
  ())

(defclass effect-plugin-module (effect-plugin-model plugin-module)
  ())

(defmethod initialize-instance :after ((self plugin-module) &key)
  (let ((button (make-instance 'button :label "Open" :x *layout-space*
                                       :y (+ *font-size* (* *layout-space* 2)))))
    (add-child self button)
    (defmethod click ((button (eql button)) btn x y)
      (open-editor self))))

(defclass op-add-module (op-add module)
  ())

(defclass op-multi-module (op-multi module)
  ())

(defclass volume-controller-mixin ()
  ((volume-slider :initarg :volume-slide :accessor .volume-slider)))

(defmethod initialize-instance :after ((self volume-controller-mixin) &key)
  (add-child self
             (setf (.volume-slider self)
                   (make-instance 'slider
                                  :value (lambda () (.volume self))
                                  :onchange (lambda (x) (setf (.volume self) x)))))
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

(defclass gain-module (gain volume-controller-mixin module)
  ()
  (:default-initargs :height 45))

(defmethod serialize ((self gain-module))
  `((setf (.volume x) ,(.volume self))
    ,@(call-next-method)))

(defclass master-module (master volume-controller-mixin module)
  ()
  (:default-initargs  :name "Master" :x 695 :y 515
                        :color (list #xff #xa5 #x00 *transparency*)))

(defmethod initialize-instance :after ((self master-module) &key)
  (add-child self
             (make-instance 'label
                            :x 25 :y 45
                            :value (let ((f (interval-upadate-value
                                             (lambda () (.last-left self))
                                             0.3)))
                                     (lambda ()
                                       (format nil "~,5f" (funcall f))))))
  (add-child self
             (make-instance 'label
                            :x 25 :y 60
                            :value (let ((f (interval-upadate-value
                                             (lambda () (.last-right self))
                                             0.3)))
                                     (lambda ()
                                       (format nil "~,5f" (funcall f)))))))

(defmethod serialize ((self master-module))
  `((setf (.volume x) ,(.volume self))
    ,@(call-next-method)))

(defclass menu-view (render-border-mixin view)
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
          in `(("Pattern" pattern-module)
               ("Sin" sin-osc-module)
               ("Saw" saw-osc-module)
               ("Adsr" adsr-module)
               ("Op Add" op-add-module)
               ("Op Multi" op-multi-module)
               ("Gain" gain-module))
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
        ((sdl2:scancode= scancode :scancode-return)
         (awhen (car (.children self))
           (click it sdl2-ffi:+sdl-button-left+ 0 0)))
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
  (let ((module (apply #'make-instance (.class self)
                    :name (.label self)
                    :x (- (.mouse-x *app*) 10)
                    :y (- (.mouse-y *app*) 10)
                    (.initargs self))))
    (addend-view module)
    (setf (.selected-module *app*) module))
  (close (.root-parent self)))

(defclass menu-plugin-button (button)
  ((plugin-description :initarg :plugin-description
                       :accessor .plugin-description)))

(defmethod click ((self menu-plugin-button) (button (eql 1)) x y)
  (let* ((plugin-description (.plugin-description self))
         (class (if (.is-instrument plugin-description)
                    'instrument-plugin-module
                    'effect-plugin-module))
         (module (make-instance class
                                :name (.name plugin-description)
                                :x (- (.mouse-x *app*) 10)
                                :y (- (.mouse-y *app*) 10)
                                :plugin-description plugin-description)))
    (addend-view module)
    (setf (.selected-module *app*) module)
    (close (.root-parent self))))

(defun open-menu ()
  (let ((module (make-instance 'menu-view
                               :x (- (.mouse-x *app*) 10)
                               :y (- (.mouse-y *app*) 10))))
    (addend-view module)
    (setf (.selected-module *app*) module)))
