(in-package :colitrsynth)

(defmethod render :around ((self view) renderer)
  (when (show-p self)
    (call-next-method)))

(defun to-bind-mod-value (mod-value)
  (+ (if (plusp (logand mod-value sdl2-ffi:+kmod-ctrl+))
         +ctrl+
         0)
     (if (plusp (logand mod-value sdl2-ffi:+kmod-alt+))
         +alt+
         0)
     (if (plusp (logand mod-value sdl2-ffi:+kmod-shift+))
         +shift+
         0)))

(defmethod mousebuttondown :after (self (button (eql sdl2-ffi:+sdl-button-right+))
            state clicks x y)
    (setf (.cable-src *app*) nil))



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



(defmethod .value ((self function-value-mixin))
  (let ((value (slot-value self 'value)))
    (if (functionp value)
        (funcall value)
        value)))

(defmethod (setf .value) (value (self function-value-mixin))
  (setf (slot-value self 'value) value))



(defmethod close ((self view) &key abort)
  (declare (ignore abort)))

(defun click-target-module (button)
  (aref (slot-value *app* 'click-target-module) button))

(defun (setf click-target-module) (value button)
  (setf (aref (slot-value *app* 'click-target-module) button) value))

(defun add-view (view)
  (push view (.views *app*)))

(defun append-view (view)
  (setf (.views *app*)
        (append (.views *app*) (list view))))

(defun remove-view (module)
  (setf (.views *app*) (remove module (.views *app*))))

(defmethod at-x-y-p ((self view) x y)
  (and (<= (.screen-x self) x (+ (.screen-x self) (.width self)))
       (<= (.screen-y self) y (+ (.screen-y self) (.height self)))))

(defun view-at-mouse (app)
  (loop for view in (reverse (.views app))
          thereis (and (at-x-y-p view (.mouse-x app)(.mouse-y app))
                       view)))

(defmethod child-view-at ((self view) x y)
  (loop for view in (.children self)
          thereis (and (<= (.x view) x (+ (.x view) (.width view)))
                       (<= (.y view) y (+ (.y view) (.height view)))
                       view)))

(defmethod children-view-at ((self view) x y)
  (loop for view in (.children self)
        if (and (<= (.x view) x (+ (.x view) (.width view)))
                (<= (.y view) y (+ (.y view) (.height view))))
          collect view))

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

(defmethod bounds ((self view))
  (values (.x self)
          (.y self)
          (+ (.x self) (.width self))
          (+ (.y self) (.height self))))

(defmethod bounds ((self partial-view))
  (children-bounds self))

(defmethod children-bounds ((self view))
  (let ((x1 most-positive-fixnum)
        (y1 most-positive-fixnum)
        (x2 0)
        (y2 0))
    (loop for child in (.children self)
          do (multiple-value-bind (ax1 ay1 ax2 ay2)
                 (bounds child)
               (setf x1 (min x1 ax1))
               (setf y1 (min y1 ay1))
               (setf x2 (max x2 ax2))
               (setf y2 (max y2 ay2))))
    (if (= x1 most-positive-fixnum)
        (values 0 0 0 0)
        (values x1 y1 x2 y2))))

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
  (or (call-next-method)
      (loop for child in (children-view-at self x y)
            do (click child button
                      (translate-child-x self child x)
                      (translate-child-y self child y)))))

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
  (let  ((direction (.drag-resize-direction *app*)))
    (when (member direction '(:north-west :west :south-west))
      (incf (.x self) xrel)
      (when (< (decf (.width self) xrel) 20)
        (decf (.x self) (- 20 (.width self)))
        (setf (.width self) 20)))
    (when (member direction '(:north-east :east :south-east))
      (setf (.width self) (max 20 (+ (.width self) xrel))))
    (when (member direction '(:north-west :north :north-east))
      (incf (.y self) yrel)
      (when (< (decf (.height self) yrel) 20)
        (decf (.y self) (- 20 (.height self)))
        (setf (.height self) 20)))
    (when (member direction '(:south-east :south :south-west))
      (setf (.height self) (max 20 (+ (.height self) yrel))))))

(defmethod add-child ((parent view) (child view))
  (unless (eq parent (.parent child))
    (setf (.children parent) (append (.children parent) (list child)))
    (setf (.parent child) parent)))

(defmethod remove-child ((parent view) (child view))
  (setf (.children parent) (remove child (.children parent)))
  (setf (.parent child) nil))

(defmethod .x ((self null))
  0)

(defmethod .y ((self null))
  0)

(defmethod .render-x ((self null))
  0)

(defmethod .render-y ((self null))
  0)

(defmethod .render-x ((self view))
  (+ (.x self)
     (if (typep (.parent self) 'partial-view)
         0
         (.render-x (.parent self)))))

(defmethod .screen-x ((self null))
  0)

(defmethod .screen-x ((self view))
  (+ (.x self)
     (.screen-x (.parent self))))

(defmethod .render-y ((self view))
  (+ (.y self)
     (if (typep (.parent self) 'partial-view)
         0
         (.render-y (.parent self)))))

(defmethod .screen-y ((self null))
  0)

(defmethod .screen-y ((self view))
  (+ (.y self)
     (.screen-y (.parent self))))

(defmethod .center-x ((self view))
  (round (/ (.width self) 2)))

(defmethod .render-center-x ((self view))
  (+ (.render-x self)
     (.center-x self)))

(defmethod .screen-center-x ((self view))
  (+ (.screen-x self)
     (.center-x self)))

(defmethod .center-y ((self view))
  (round (/ (.height self) 2)))

(defmethod .render-center-y ((self view))
  (+ (.render-y self)
     (.center-y self)))

(defmethod .screen-center-y ((self view))
  (+ (.screen-y self)
     (.center-y self)))

(defmethod translate-child-x ((self view) (child view) x)
  (- x (.x child)))

(defmethod translate-child-y ((self view) (child view) y)
  (- y (.y child)))

(defmethod render ((self view) renderer)
  (loop for child in (.children self)
        do (render child renderer))
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
                        (- (.mouse-x *app*) (.render-x self))
                        (- (.mouse-y *app*) (.render-y self)))
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
                   (values (- x (.render-x self))
                           (- y (.render-y self)))))))
    (multiple-value-bind (x y) (sdl2:mouse-state)
      (f view x y))))

(defmethod render ((self render-border-mixin) renderer)
  (apply #'sdl2:set-render-draw-color renderer (.color self))
  (sdl2:render-draw-rect renderer
                         (sdl2:make-rect (.render-x self)
                                         (.render-y self)
                                         (.width self)
                                         (.height self)))
  (call-next-method))


(defmethod click ((self connector)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (let ((cable-src (.cable-src *app*)))
    (cond ((null cable-src)
           (let ((available-cables (available-cables-src (.module self))))
             (if (ctrl-key-p)
                 (open-output-menu available-cables)
                 (setf (.cable-src *app*) (car available-cables)))))
          ((eq (.module self) (.src cable-src)))
          (t
           (let* ((src (.src cable-src))
                  (dest (.module self))
                  (available-connections (available-connections src dest cable-src)))
             (when available-connections
               (if (ctrl-key-p)
                   (open-connector-menu available-connections)
                   (let ((connection (car available-connections)))
                     (if (connected-p connection)
                         (disconnect connection)
                         (connect connection)))))
             (setf (.cable-src *app*) nil)))))
  t)

(defmethod resized ((self connector))
  (let ((module (.module self)))
    (setf (.x self) (- (.width module) 7))
    (setf (.y self) -7)
    (setf (.width self) 14)
    (setf (.height self) 14)))



(defmethod at-x-y-p ((self connector-mixin) x y)
  (or (call-next-method)
      (at-x-y-p (.connector self) x y)))

(defmethod initialize-instance :after ((self connector-mixin) &key)
  (let ((connector (make-instance 'connector :module self)))
    (setf (.connector self) connector)
    (add-child self connector)))


(defmethod initialize-instance :after ((self module) &key)
  (initialize self))

(defmethod initialize-instance :around ((self module) &key)
  (call-next-method)
  (resized self))

(defmethod initialize ((self module))
  (unless (typep self 'sequencer-module) ;?????????????????????
    (add-child self
               (make-instance 'text
                              :reader (lambda () (.name self))
                              :writer (lambda (value) (setf (.name self) value))
                              :x *layout-space*
                              :y *layout-space*))))

(defmethod close ((self module) &key abort)
  (declare (ignore abort)))

(defmethod mousebuttondown :after ((self module)
                                   (button (eql sdl2-ffi:+sdl-button-right+))
                                   state clicks x y)
  (setf (.selected-modules *app*) nil)
  (setf (.views *app*)
        (cons self (delete self (.views *app*)))))

(defmethod serialize ((self model))
  `((setf (.name x) ,(.name self)
          (.in x) ,(serialize
                    (loop for x in (.in self)
                          if (member (.src x) (.selected-modules *app*))
                            collect x))
          (.out x) ,(serialize
                     (loop for x in (.out self)
                           if (member (.dest x) (.selected-modules *app*))
                             collect x)))
    ,@(call-next-method)))

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

(let ((old-cursor nil))
  (defun set-cursor (cursor-type)
    (let* ((cursor-id
             (case cursor-type
               ((:north-west :south-east)
                sdl2-ffi:+sdl-system-cursor-sizenwse+)
               ((:north-east :south-west)
                sdl2-ffi:+sdl-system-cursor-sizenesw+)
               ((:west :east)
                sdl2-ffi:+sdl-system-cursor-sizewe+)
               ((:north :south)
                sdl2-ffi:+sdl-system-cursor-sizens+)
               (t
                sdl2-ffi:+sdl-system-cursor-arrow+)))
           (new-cursor (sdl2-ffi.functions:sdl-create-system-cursor cursor-id)))
      (sdl2-ffi.functions:sdl-set-cursor new-cursor)
      (when old-cursor
        (sdl2-ffi.functions:sdl-free-cursor old-cursor)
        (setf old-cursor new-cursor)))))

(defmethod mousemotion ((self drag-resize-mixin) x y xrel yrel state)
  (let ((drag-state (.drag-state *app*)))
    (when (or (null drag-state) (not (.dragging drag-state)))
      (let ((direction
              (cond ((and (<= x 5) (<= y 5))
                     :north-west)
                    ((and (<= (- (.width self) 5) x) (<= y 5))
                     :north-east)
                    ((and (<= (- (.width self) 5) x) (<= (- (.height self) 5) y))
                     :south-east)
                    ((and (<= x 5) (<= (- (.height self) 5) y))
                     :south-west)
                    ((<= x 5)
                     :west)
                    ((<= y 5)
                     :north)
                    ((<= (- (.width self) 5) x)
                     :east)
                    ((<= (- (.height self) 5) y)
                     :south)
                    (t nil))))
        (setf (.drag-resize-direction *app*) direction)
        (set-cursor direction))))
  (call-next-method))

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

(defmethod drag ((self drag-move-mixin) xrel yrel (button (eql 1)))
  (loop for module in (.selected-modules *app*)
        if (typep module 'drag-move-mixin)
          do (move module xrel yrel)))

(defmethod drag-start ((self drag-resize-mixin) x y (button (eql 1)))
  (when (.drag-resize-direction *app*)
    (setf (.drag-resize-module *app*) self)))

(defmethod drag ((self drag-resize-mixin) xrel yrel (button (eql 1)))
  (if (eq self (.drag-resize-module *app*))
      (loop for module in (.selected-modules *app*)
            if (typep module 'drag-resize-mixin)
              do (resize module xrel yrel))
      (call-next-method)))

(defmethod drag-end ((self drag-resize-mixin) x y (button (eql 1)))
  (when (eq self (.drag-resize-module *app*))
    (setf (.drag-resize-module *app*) nil)
    (sdl2-ffi.functions:sdl-set-cursor
     (sdl2-ffi.functions:sdl-create-system-cursor
      sdl2-ffi:+sdl-system-cursor-arrow+)))
  (call-next-method))

(defmethod disconnect-all ((self module))
  (disconnect-all self))


(defun compute-connection-points (from to offset)
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
    (let* ((x1 (.screen-center-x from))
           (y1 (.screen-center-y from))
           (x2 (.screen-center-x to))
           (y2 (.screen-center-y to))
           (xs1 (.screen-x from))
           (ys1 (.screen-y from))
           (xs2 (+ xs1 (.width from)))
           (ys2 ys1)
           (xs3 xs2)
           (ys3 (+ ys2 (.height from)))
           (xs4 xs1)
           (ys4 ys3)
           (xe1 (.screen-x to))
           (ye1 (.screen-y to))
           (xe2 (+ xe1 (.width to)))
           (ye2 ye1)
           (xe3 xe2)
           (ye3 (+ ye2 (.height to)))
           (xe4 xe1)
           (ye4 ye3))
      (destructuring-bind (xs . ys)
          (or (aif (intersec x1 y1 x2 y2 xs1 ys1 xs2 ys2)
                   (cons (+ (car it) offset) (cdr it)))
              (aif (intersec x1 y1 x2 y2 xs2 ys2 xs3 ys3)
                   (cons (car it) (+ (cdr it) offset)))
              (aif (intersec x1 y1 x2 y2 xs3 ys3 xs4 ys4)
                   (cons (+ (car it) offset) (cdr it)))
              (aif (intersec x1 y1 x2 y2 xs4 ys4 xs1 ys1)
                   (cons (car it) (+ (cdr it) offset)))
              (cons x1 y1))
        (destructuring-bind (xe . ye)
            (or (intersec x1 y1 x2 y2 xe1 ye1 xe2 ye2)
                (intersec x1 y1 x2 y2 xe2 ye2 xe3 ye3)
                (intersec x1 y1 x2 y2 xe3 ye3 xe4 ye4)
                (intersec x1 y1 x2 y2 xe4 ye4 xe1 ye1)
                (cons x2 y2))
          (values xs ys xe ye))))))

(defmethod cable-color ((self connection))
  *connection-line-color*)

(defmethod cable-color ((self audio-connection))
  (if (= (.dest-bus self) 0)
      *connection-line-color*
      *cable-color-audio-aux*))

(defmethod cable-color ((self midi-connection))
  *cable-color-midi*)

(defmethod cable-color ((self param-connection-mixin))
  *cable-color-param*)

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
        (let ((surface (render-font self)))
          (setf (.width self) (sdl2:surface-width surface)
                (.height self) (sdl2:surface-height surface)
                (.texture self) (sdl2:create-texture-from-surface renderer surface))))
      (sdl2:render-copy renderer
                        (.texture self)
                        :source-rect nil
                        :dest-rect (sdl2:make-rect (.render-x self) (.render-y self)
                                                   (.width self) (.height self))))))

(defmethod render-font ((self label))
  (apply #'sdl2-ttf:render-utf8-solid
         (.font *app*)
         (.value self)
         (.color self)))

(defmethod render-font ((self muliti-line-label))
  (apply #'sdl2-ttf:render-utf8-solid-wrapped
         (.font *app*)
         (.value self)
         (.width self)
         (.color self)))


(defmethod initialize-instance :after ((self button) &key label)
  (let ((label (make-instance 'label :value label :x 5 :y 2)))
    (add-child self label)
    (setf (.label-view self) label)
    (setf (.width self) (+ 10 (.width label))
          (.height self) (+ 4 (.height label)))))

(defmethod .label ((self button))
  (.value (.label-view self)))

(defmethod render :after ((self button) renderer)
  ;; after ??????????????????????????????????????????????????????????????????
  (let ((label (.label-view self)))
    (setf (.width self) (+ 10 (.width label))
          (.height self) (+ 4 (.height label)))))


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


(defmethod initialize-instance :after ((self text) &key)
  (setf (.label self) (make-instance 'label
                                     :value (lambda () (.edit-buffer self))
                                     :x 3 :y 1))
  (setf (.cursor-position self) (length (.edit-buffer self)))
  (add-child self (.label self)))

(defmethod click ((self text) button x y)
  ;; single click ??????????????????????????????
  nil)

(defmethod .edit-buffer :around ((self text))
  (if (.focused self)
      (call-next-method)
      (setf (.edit-buffer self) (funcall (.reader self)))))

(defmethod double-click ((self text) button x y)
  (unless (eq self (.focused-view *app*))
    (setf (.focused-view *app*) self))
  (call-next-method))

(defmethod render ((self text) renderer)
  (let ((cursor-x (+ (.render-x self)
                     2
                     (* *char-width* (.cursor-position self))))
        (cursor-y (+ (.render-y self) 2))
        (cursor-w *char-width*)
        (cursor-h *char-height*))
    (when (.focused self)
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (sdl2:render-fill-rect renderer
                             (sdl2:make-rect cursor-x cursor-y
                                             cursor-w cursor-h))
      (apply #'sdl2:set-render-draw-color renderer *focused-color*)
      (sdl2:render-draw-rect renderer
                             (sdl2:make-rect (.render-x self)
                                             (.render-y self)
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

(defmethod initialize-instance :after ((self slider) &key)
  (add-child self (make-instance 'label :value (lambda () (format nil "~,5f" (.value self)))
                                       :x *layout-space* :y (round (/ *layout-space*)))))

(defmethod render ((self slider) renderer)
  (call-next-method)
  ;; TODO ??????????????? resized ????????????????????????????????????
  (let ((value (.value self))
        (f (.compute-function self)))
    (multiple-value-bind (_ min-x) (funcall f (.min self) 0)
      (declare (ignore _))
      (multiple-value-bind (_ max-x)  (funcall f (.max self) 0)
        (declare (ignore _))
        (multiple-value-bind (_ current-x) (funcall f value 0)
          (declare (ignore _))
          (let ((x (+ (round (* (.width self)
                                (/ (- current-x min-x) (- max-x min-x))))
                      (.render-x self)))
                (y (.render-y self)))
            (sdl2:set-render-draw-color renderer #xff #x00 #xff #xff)
            (sdl2:render-draw-line renderer
                                   x (1+ y)
                                   x (+ y (.height self) -2))))))))

(defmethod drag ((self slider) xrel yrel button)
  (funcall (.onchange self)
           (min (.max self)
                (max (.min self)
                     (funcall (.compute-function self)
                              (.value self)
                              (/ xrel (if (ctrl-key-p)
                                          100
                                          1)))))))


(defmethod child-view-at ((self partial-view) x y)
  (loop for view in (.children self)
        ;; ????????? x ???????????????????????????????????????
          thereis (and (<= (.x view) x (+ (.x view) (.width view)))
                       (<= (.y view) (+ y (.offset-y self)) (+ (.y view) (.height view)))
                       view)))

(defmethod children-view-at ((self partial-view) x y)
  (loop for view in (.children self)
        ;; ????????? x ???????????????????????????????????????
        if (and (<= (.x view) x (+ (.x view) (.width view)))
                (<= (.y view) (+ y (.offset-y self)) (+ (.y view) (.height view))))
          collect view))

(defmethod render ((self partial-view) renderer)
  (let* ((texture-width (.texture-width self))
         (texture-height (.texture-height self))
         (texture (sdl2:create-texture renderer :rgba8888 :target
                                       texture-width texture-height)))
    (unwind-protect
         (let ((original-renderer-target
                 (sdl2-ffi.functions:sdl-get-render-target renderer)))
           (sdl2:set-render-target renderer texture)
           (sdl2:set-texture-blend-mode texture :add)
           (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #xff)
           (sdl2:render-clear renderer)

           (call-next-method)

           (sdl2:set-render-target renderer original-renderer-target)
           (let* ((src-x (.offset-x self))
                  (src-y (max 0 (.offset-y self)))
                  (dst-x (.render-x self))
                  (dst-y (- (.render-y self) (min 0 (.offset-y self))))
                  (dst-w (.width self))
                  (src-w dst-w)
                  (src-h (+ (.height self) (min 0 (.offset-y self))))
                  (dst-h (+ (.height self) (min 0 (.offset-y self))
                            (min 0 (- texture-height (.height self)  (.offset-y self)))))
                  (dst-rect (sdl2:make-rect dst-x dst-y dst-w dst-h))
                  (src-rect (sdl2:make-rect src-x src-y src-w src-h)))
             (sdl2:render-copy renderer texture :source-rect src-rect :dest-rect dst-rect)))
      (sdl2:destroy-texture texture))))

(defmethod .screen-center-x ((self partial-view))
  (- (call-next-method) (.offset-x self)))

(defmethod .screen-center-y ((self partial-view))
  (- (call-next-method) (.offset-y self)))

(defmethod .screen-x ((self partial-view))
  (- (call-next-method) (.offset-x self)))

(defmethod .screen-y ((self partial-view))
  (- (call-next-method) (.offset-y self)))

(defmethod .texture-height ((self partial-view))
  (max (loop for child in (.children self)
             maximize (+ (.y child) (.height child)))
       (.height self)))

(defmethod .texture-width ((self partial-view))
  (max (loop for child in (.children self)
             maximize (+ (.x child) (.width child)))
       (.width self)))

(defmethod wheel ((self partial-view) delta)
  (multiple-value-bind (x1 y1 x2 y2) (children-bounds self)
    (declare (ignore x1 y1))
    (if (.shift-key-p *app*)
        (progn
          (setf (.offset-x self)
                (max 0 (min
                        (- (.offset-x self) (* 5 delta))
                        (- x2 (+ (.render-x self) (.width self))))))
          (resized self))               ;connector ???????????????
        (setf (.offset-y self)
              (max 0 (min
                      (- (.offset-y self) (* 5 delta))
                      (- y2 (.height self))))))))

(defmethod translate-child-x ((self partial-view) (child view) x)
  (+ (call-next-method) (.offset-x self)))

(defmethod translate-child-y ((self partial-view) (child view) y)
  (+ (call-next-method) (.offset-y self)))

(defmethod initialize-instance :after ((self volume-controller-mixin) &key)
  (add-child self
             (setf (.volume-slider self)
                   (make-instance 'slider
                                  :max 2.0
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
