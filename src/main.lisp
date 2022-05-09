(in-package :colitrsynth)

(defparameter *font-size* 14)
(defparameter *char-width* (/ *font-size* 2))
(defparameter *char-height* *font-size*)
(defparameter *cursor-color* '(#x00 #x00 #xcc #x80))
(defparameter *play-position-color* '(#x00 #x80 #x00 #x80))
(defparameter *pixcel-per-line* 5)
(defparameter *layout-space* 5)

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
      (let ((root (root-parent self)))
        (click root button
               (- (.mouse-x *app*) (.absolute-x root))
               (- (.mouse-y *app*) (.absolute-y root)))))))

(defgeneric click (self button x y)
  (:method (self button x y)))

(defgeneric drag-start (self x y)
  (:method (self x y)))

(defgeneric drag (self xrel yrel)
  (:method (self xrel yrel)))

(defgeneric drag-end (self x y)
  (:method (self x y)))

(defgeneric drop (self dropped x y)
  (:method (self dropped x y)))

(defgeneric mousemotion (self x y xrel yrel state)
  (:method (self x y xrel yrel state)))

(defgeneric keydown (self scancode mod-value)
  (:method (self scancode mod-value)))

(defgeneric keyup (self scancode mod-value)
  (:method (self scancode mod-value)))

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
   (click-target-module :initform (make-array +mouse-button-count+))
   (drag-move-module :initform nil :accessor .drag-move-module)
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
   (dragged :initform nil :accessor .dragged)))

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

(defmethod root-parent ((self renderable))
  (aif (.parent self)
       (root-parent it)
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

(defmethod drag ((self renderable) xrel yrel)
  (move self xrel yrel))

(defmethod drop ((self renderable) dropped x y)
  (call-next-method)
  (awhen (child-module-at self x y)
    (drop it dropped (- x (.x it)) (- y (.y it)))))

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
  (setf (.children parent) (append (.children parent) (list child)))
  (setf (.parent child) parent))

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

(defmethod .y ((self null))
  0)

(defmethod .absolute-y ((self null))
  0)

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
  (sdl2:set-render-draw-color renderer #x22 #x8b #x22 *transparency*)
  ;; TODO そのうち直す
  (loop for out in (.out self)
        do (let (x1 y1 x2 y2)
             (if (> (abs (- (.absolute-x self) (.absolute-x out)))
                    (abs (- (.absolute-y self) (.absolute-y out))))
                 (if (< (.absolute-x self) (.absolute-x out))
                     (if (< (.absolute-y self) (.absolute-y out))
                         (progn
                           (setf x1 (+ (.absolute-x self) (.width self)))
                           (setf y1 (+ (.absolute-y self) (/ (.height self) 2)))
                           (setf x2 (.absolute-x out))
                           (setf y2 (+ (.absolute-y out) (/ (.height out) 2))))
                         (progn
                           (setf x1 (+ (.absolute-x self) (.width self)))
                           (setf y1 (+ (.absolute-y self) (/ (.height self) 2)))
                           (setf x2 (.absolute-x out))
                           (setf y2 (+ (.absolute-y out) (/ (.height out) 2)))))
                     (if (< (.absolute-y self) (.absolute-y out))
                         (progn
                           (setf x1 (.absolute-x self))
                           (setf y1 (+ (.absolute-y self) (/ (.height self) 2)))
                           (setf x2 (+ (.absolute-x out) (.width out)))
                           (setf y2 (+ (.absolute-y out) (/ (.height out) 2))))
                         (progn
                           (setf x1 (.absolute-x self))
                           (setf y1 (+ (.absolute-y self) (/ (.height self) 2)))
                           (setf x2 (+ (.absolute-x out) (.width out)))
                           (setf y2 (+ (.absolute-y out) (/ (.height out) 2))))))
                 (if (< (.absolute-x self) (.absolute-x out))
                     (if (< (.absolute-y self) (.absolute-y out))
                         (progn
                           (setf x1 (+ (.absolute-x self) (/ (.width self) 2)))
                           (setf y1 (+ (.absolute-y self) (.height self)))
                           (setf x2 (+ (.absolute-x out) (/ (.width out) 2)))
                           (setf y2 (.absolute-y out)))
                         (progn
                           (setf x1 (+ (.absolute-x self) (/ (.width self) 2)))
                           (setf y1 (.absolute-y self))
                           (setf x2 (+ (.absolute-x out) (/ (.width out) 2)))
                           (setf y2 (+ (.absolute-y out) (.height out)))))
                     (if (< (.absolute-y self) (.absolute-y out))
                         (progn
                           (setf x1 (+ (.absolute-x self) (/ (.width self) 2)))
                           (setf y1 (+ (.absolute-y self) (.height self)))
                           (setf x2 (+ (.absolute-x out) (/ (.width out) 2)))
                           (setf y2 (.absolute-y out)))
                         (progn
                           (setf x1 (+ (.absolute-x self) (/ (.width self) 2)))
                           (setf y1 (.absolute-y self))
                           (setf x2 (+ (.absolute-x out) (/ (.width out) 2)))
                           (setf y2 (+ (.absolute-y out) (.height out)))))))
             (setf x1 (floor x1)
                   x2 (floor x2)
                   y1 (floor y1)
                   y2 (floor y2)) 
             (sdl2:render-draw-line renderer x1 y1 x2 y2)
             (sdl2:render-draw-rect renderer (sdl2:make-rect (- x1 2) (- y1 2) 5 5))))
  (call-next-method))

(defclass name-mixin ()
  ((name :initarg :name :initform "noname" :accessor .name)))

(defmethod initialize-instance :after ((self name-mixin) &key)
  (let ((text (make-instance 'text :value (.name self) :x 3 :y 3)))
    (add-child self text)
    (defmethod (setf .name) (value (self (eql self)))
      (setf (.value text) value))))

(defclass module (name-mixin
                  drag-resize-mixin     ;drag-move-mixin より先に
                  drag-move-mixin
                  renderable
                  drag-connect-mixin)
  ())

(defmethod mousebuttondown :before ((self module) button state clicks x y)
  (setf (.selected-module *app*) self))

(defmethod render :after ((self module) renderer)
  (when (eq self (.selected-module *app*))
    (sdl2:set-render-draw-color renderer #xff #xff #x00 #xff)
    (sdl2:render-draw-rect
     renderer
     (sdl2:make-rect (- (.absolute-x self) 2)
                     (- (.absolute-y self) 2)
                     (+ (.width self) 4)
                     (+ (.height self) 4)))))

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
          (sunless (.dragged drag-state)
            (setf it t)
            (drag-start self x y))
          (drag self xrel yrel))
        (call-next-method))))

(defmethod mousebuttonup ((self drag-mixin) button state clicks x y)
  (if (eq self (.target (.drag-state *app*)))
      (progn
        (if (.dragged (.drag-state *app*))
            (progn (drag-end self x y)
                   (drop (module-at-mouse *app*) self
                         (+ (.absolute-x self) x)
                         (+ (.absolute-y self) y)))
            (call-next-method))
        (setf (.drag-state *app*) nil))
      (call-next-method)))

(defclass drop-mixin ()
  ())

(defclass drag-move-mixin ()
  ())

(defmethod mousebuttondown ((self drag-move-mixin) button state clicks x y)
  (case button
    (1 (setf (.drag-move-module *app*) self)))
  (call-next-method))

(defmethod mousebuttonup ((self drag-move-mixin) button state clicks x y)
  (if (and (= button 1)
           (eq self (.drag-move-module *app*))
           (.dragging *app*))
      nil
      (call-next-method)))

(defmethod mousemotion ((self drag-move-mixin) x y xrel yrel state)
  (when (eq self (.drag-move-module *app*))
    (setf (.dragging *app*) t)
    (move self xrel yrel))
  (call-next-method))

(defclass drag-resize-mixin ()
  ())

(defmethod mousebuttondown ((self drag-resize-mixin) button state clicks x y)
  (if (and (= button 1)
           (< (.width self) (+ 10 x))
           (< (.height self) (+ 10 y)))
      (setf (.drag-resize-module *app*) self)
      (call-next-method)))

(defmethod mousebuttonup ((self drag-resize-mixin) button state clicks x y)
  (if (and (= button 1)
           (eq self (.drag-resize-module *app*))
           (.dragging *app*))
      nil
      (call-next-method)))

(defmethod mousemotion ((self drag-resize-mixin) x y xrel yrel state)
  (when (eq self (.drag-resize-module *app*))
    (setf (.dragging *app*) t)
    (resize self xrel yrel))
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

(defmethod mousebuttondown ((self drag-connect-mixin) button state clicks x y)
  (case button
    (3 (setf (.connect-from-module *app*) self)))
  (call-next-method))

(defmethod mousebuttonup ((self drag-connect-mixin) button state clicks x y)
  (case button
    (3 (let ((from (.connect-from-module *app*)))
         (when (and from (not (eq from self)))
           (if (member self (.out from))
               (disconnect from self)
               (connect from self)))
         (setf (.connect-from-module *app*) nil))))
  (call-next-method))

(defclass text (renderable)
  ((value :initarg :value :initform "くえっ" :accessor .value)
   (lat-value :initform "" :accessor .last-value)
   (texture :initform nil :accessor .texture))
  (:default-initargs :width 0 :height 0))

(defmethod render ((self text) renderer)
  ;; (describe self)
  ;; (print (list (.absolute-x self) (.absolute-y self)))
  (when (string/= (.value self) "")
    (when (string/= (.value self) (.last-value self))
      (setf (.last-value self) (.value self))
      (awhen (.texture self)
        (sdl2:destroy-texture it))
      (let ((surface (apply #'sdl2-ttf:render-utf8-solid (.font *app*)
                            (.value self)
                            (.color self))))
        (setf (.width self) (sdl2:surface-width surface)
              (.height self) (sdl2:surface-height surface)
              (.texture self) (sdl2:create-texture-from-surface renderer surface))))
    (sdl2:render-copy renderer
                      (.texture self)
                      :source-rect nil
                      :dest-rect (sdl2:make-rect (.absolute-x self) (.absolute-y self)
                                                 (.width self) (.height self)))))

(defclass button (renderable)
  ()
  (:default-initargs :width 50 :height 30))

(defmethod initialize-instance :after ((self button) &key text)
  (add-child self (make-instance 'text :value text :x 5 :y 2)))

(defmethod render :after ((self button) renderer)
  ;; after でやるので初回描画時は崩れてるはずだけど妥協
  (let ((text (car (.children self))))
    (setf (.width self) (+ 10 (.width text))
          (.height self) (+ 4 (.height text)))))

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
  (let ((texture (sdl2:create-texture renderer :rgba8888 :target 800 600)))
    (unwind-protect
         (progn
           (sdl2:set-render-target renderer texture)
           (sdl2:set-texture-blend-mode texture :blend)
           (sdl2:set-render-draw-color renderer 0 0 0 #x00)
           (sdl2:render-clear renderer)
           ;; play position
           (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
           (sdl2:render-fill-rect
            renderer
            (sdl2:make-rect (+ (* (.cursor-x self) *char-width*)  (.absolute-x self) 2)
                            (+ (* (.current-line (.pattern self)) *char-height*) (.absolute-y self) 2)
                            (.width self)
                            *char-height*))
           ;; cursor position
           (when (eq (.parent self) (module-at-mouse *app*))
             (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
             (sdl2:render-fill-rect
              renderer
              (sdl2:make-rect (+ (* (.cursor-x self) *char-width*)  (.absolute-x self) 2)
                              (+ (* (.cursor-y self) *char-height*) (.absolute-y self) 2)
                              (if (zerop (.cursor-x self)) (* 3 *char-width*) *char-width*)
                              *char-height*)))
           (call-next-method)
           (sdl2:set-render-target renderer nil)
           (let ((rect (sdl2:make-rect (.absolute-x self) (.absolute-y self)
                                       (.width self) (.height self))))
             (sdl2:render-copy renderer texture :source-rect rect :dest-rect rect)))
      (sdl2:destroy-texture texture))))

(defmethod keydown ((self pattern-editor) scancode mod-value)
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
     (let ((module (.selected-module *app*)))
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

(defmethod drop ((self sequencer-module-track) (pattern-position pattern-position) x y)
  (let ((delta (- (pixcel-to-line (.x pattern-position)) (.start pattern-position))))
    (incf (.start pattern-position) delta)
    (incf (.end pattern-position) delta)
    (update-sequencer-end)))

(defclass pattern-position (pattern-position-mixin
                            drag-mixin
                            renderable
                            name-mixin)
  ((move-delta-x :initform 0 :accessor .move-delta-x)))

(defmethod drag ((self pattern-position) xrel yrel)
  (let* ((pixcel (+ (.x self) (.move-delta-x self) xrel))
         (line (pixcel-to-line pixcel))
         (rounded-pixcel (line-to-pixcel line)))
    (setf (.x self) rounded-pixcel
          (.move-delta-x self) (- pixcel rounded-pixcel))))

(defmethod drag-end ((self pattern-position) x y)
  (setf (.move-delta-x self) 0))

(defclass sequencer-module (sequencer
                            drag-mixin
                            drag-resize-mixin     ;drag-move-mixin より先に
                            name-mixin
                            renderable)
  ()
  (:default-initargs :name ""
                     :color (list #x00 #xff #xff *transparency*)
                     :width 700
                     :height 200))

(defmethod initialize-instance :after ((self sequencer-module) &key)
  (let ((play-button (make-instance 'button :text "▶" :x 5 :y 5)))
    (add-child self play-button)
    (defmethod mousebuttondown ((self (eql play-button)) button state clicks x y)
      (when (= button 1)
        (if (.playing *audio*)
            (stop)
            (play))))))

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

(defmethod remove-pattern ((track sequencer-module-track)
                           (pattern-position pattern-position))
  (remove-child track pattern-position)
  (call-next-method))

(defmethod keydown ((self pattern-module) scancode mod-value)
  (keydown (.pattern-editor self) scancode mod-value))

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
  (:default-initargs :name "adsr"))

(defclass amp-module (amp module)
  ()
  (:default-initargs :name "amp"))

(defclass master-module (master module)
  ()
  (:default-initargs :name "master" :color (list #xff #xa5 #x00 *transparency*)))

(defclass new-module-menu (renderable)
  ()
  (:default-initargs :width 100 :height 200))

(defmacro new-module-menu-button (name class &rest initargs)
  `(let ((button (make-instance 'button :text ,name
                                        :y (* (+ *font-size* 4)
                                              (length (.children self))))))
     (add-child self button)
     (defmethod click ((self (eql button)) button x y)
       (add-module (make-instance ',class :name ,name
                                          :x (- (.mouse-x *app*) 10)
                                          :y (- (.mouse-y *app*) 10)
                                          ,@initargs)))))

(defmethod initialize-instance :after ((self new-module-menu) &key)
  (new-module-menu-button "pattern" pattern-module :length 8)
  (new-module-menu-button "sin" sin-osc-module)
  (new-module-menu-button "saw" saw-osc-module)
  (new-module-menu-button "adsr" adsr-module)
  (new-module-menu-button "amp" amp-module))

(defun open-new-module-menu ()
  (add-module (make-instance 'new-module-menu
                       :x (- (.mouse-x *app*) 10)
                       :y (- (.mouse-y *app*) 10))))

(defmethod mousebuttonup ((self new-module-menu) button state clicks x y)
  (call-next-method)
  (remove-module self))

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
            (make-test-modules)
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
                                  :x 125 :y 250))
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
                                  :x 125 :y 350))
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
  (let ((scancode (sdl2:scancode-value keysym))
        (sym (sdl2:sym-value keysym))
        (mod-value (sdl2:mod-value keysym)))
    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
            sym
            scancode
            mod-value)
    (aif (module-at-mouse *app*)
         (keydown it scancode mod-value)
         (cond ((sdl2:scancode= scancode :scancode-f)
                (open-new-module-menu))))))

(defun handle-sdl2-keyup-event (keysym)
  (let  ((scancode (sdl2:scancode-value keysym))
         (mod-value (sdl2:mod-value keysym)))
    (keyup (module-at-mouse *app*) scancode mod-value)))

(defun handle-sdl2-mousemotion-event (x y xrel yrel state)
  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
          x xrel y yrel state)
  (setf (.mouse-x *app*) x)
  (setf (.mouse-y *app*) y)
  (let ((module (or (.target (.drag-state *app*))
                    (.drag-move-module *app*)
                    (.drag-resize-module *app*)
                    (module-at-mouse *app*))))
    (mousemotion module
                 (- x (.absolute-x module)) (- y (.absolute-y module))
                 xrel yrel state)))

(defun handle-sdl2-mousebuttondown-event (button state clicks x y)
  (format t "Mouse button down button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (awhen (module-at-mouse *app*)
    (mousebuttondown it
                     button state clicks
                     (- x (.absolute-x it)) (- y (.absolute-y it)))))

(defun handle-sdl2-mousebuttonup-event (button state clicks x y)
  (format t "Mouse button up button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (aif (and (.dragging *app*)
            (or (.drag-move-module *app*)
                (.drag-resize-module *app*)))
       (mousebuttonup it button state clicks
                      (- x (.absolute-x it)) (- y (.absolute-y it)))
       (awhen (module-at-mouse *app*)
         (mousebuttonup it button state clicks
                        (- x (.absolute-x it)) (- y (.absolute-y it)))))
  (setf (.drag-move-module *app*) nil)
  (setf (.drag-resize-module *app*) nil)
  (setf (.dragging *app*) nil)
  (setf (click-target-module button) nil))

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
    (sdl2-ttf:quit)))
