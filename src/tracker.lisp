(in-package :colitrsynth)

(defclass tracks-partial-view (pattern-editor)
  ())

(defclass tracker-timeline-view-mixin (render-border-mixin view)
  ((labels :initform nil :accessor .labels))
  (:default-initargs :width 15))

(defclass tracker-timeline-left-view (tracker-timeline-view-mixin)
  ())

(defclass tracker-timeline-rigth-view (tracker-timeline-view-mixin)
  ())

(defclass tracker-partial-view (render-border-mixin  partial-view)
  ((timeline-left :initarg :timeline-left :accessor .timeline-left)
   (timeline-rigth :initarg :timeline-rigth :accessor .timeline-rigth)
   (view :initarg :view :accessor .view)))

(defclass tracker (model module)
  ((bpm :initarg :bpm :initform 140.0 :accessor .bpm
        :type single-float)
   (lpb :initarg :lpb :initform 4 :accessor .lpb)
   (tracks :initarg :tracks :accessor .tracks :initform nil)
   (looping :initform t :accessor .looping)
   (loop-start-line :initform 0 :accessor .loop-start-line
                    :type fixnum)
   (loop-end-line :initform 0 :accessor .loop-end-line
                  :type fixnum)
   (play-position :initform (make-play-position)
                  :accessor .play-position)
   (last-play-position :initform (make-play-position)
                       :accessor .last-play-position)
   (view :accessor .view))
  (:default-initargs :x 15 :y 15 :width 200 :height 300
                     :color (list #xff #x00 #x00 #xff)))

(defmethod initialize-instance :after ((self tracker) &key)
  (initialize self))

(defmethod initialize ((self tracker))
  (let* ((tracks-partial-view (make-instance 'tracks-partial-view
                                             :model (make-instance 'pattern-module)))
         (timeline-left (make-instance 'tracker-timeline-left-view))
         (timeline-rigth (make-instance 'tracker-timeline-rigth-view))
         (tracker-partial-view (make-instance 'tracker-partial-view
                                              :x *layout-space*
                                              :y (+ *char-height* (* *layout-space* 4))
                                              :timeline-left timeline-left
                                              :timeline-rigth timeline-rigth
                                              :view tracks-partial-view)))
    (setf (.view self) tracker-partial-view)
    (add-child self tracker-partial-view)
    (add-child tracker-partial-view timeline-left)
    (add-child tracker-partial-view timeline-rigth)
    (add-child tracker-partial-view tracks-partial-view))
  (resized self))

(defmethod nlines ((self tracker))
  (apply #'max
         0
         (mapcar (lambda (x)
                   (apply #'max
                          (mapcar (lambda (x) (.end x))
                                  (.pattern-positions x))))
                 (.tracks self))))

(defmethod process ((self tracker) (connection null) left right))

(defmethod render ((self tracker-timeline-view-mixin) renderer)
  (let* ((tracker (tracker self))
         (loop-start (.loop-start-line tracker))
         (loop-end (.loop-end-line tracker)))
    (when (/= loop-start loop-end)
      (apply #'sdl2:set-render-draw-color renderer *loop-color*)
      (sdl2:render-fill-rect renderer
                             (sdl2:make-rect
                              (.render-x self)
                              (+ (* *pixcel-per-line* loop-start)
                                 (.render-y self))
                              (.width self)
                              (+ (* *pixcel-per-line* (- loop-end loop-start))))))
    (apply #'sdl2:set-render-draw-color renderer *default-color*)
    (loop for i from 0 to (/ (+ (nlines tracker) 16) 4)
          for y = (+ (* *pixcel-per-line* i 4 4 4) (.render-y self))
          do (sdl2:render-draw-line renderer
                                    (+ (.render-x self) (floor (/ (.width self) 2)))
                                    y
                                    (+ (.render-x self) (.width self))
                                    y))
    (call-next-method)))

(defmethod resized ((self tracker-timeline-view-mixin))
  (setf (.y self) 0)
  (setf (.height self) (* *char-height* (nlines (tracker self))))
  (call-next-method))

(defmethod resized ((self tracker-timeline-left-view))
  (call-next-method)
  (setf (.x self) 0))

(defmethod resized ((self tracker-timeline-rigth-view))
  (call-next-method)
  (setf (.x self) (- (.width (.parent self)) (.width self))))

(defmethod resized ((self tracker-partial-view))
  (let ((parent (.parent self)))
    (setf (.width self) (- (.width parent) (* *layout-space* 2)))
    (setf (.height self) (- (.height parent) *layout-space* (.y self))))
  (call-next-method))

(defmethod resized ((self tracks-partial-view))
  (let ((parent (.parent self)))
    (setf (.x self) 15)
    (setf (.y self) 0)
    (setf (.width self) (- (.width parent) 30))
    (setf (.height self) (.height parent)))
  (call-next-method))

(defmethod tracker ((self view))
  (.parent-by-class self 'tracker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcmd cmd::tracker ((self app)) (:interactive t)
  (awhen (find-if (lambda (x) (typep x 'tracker)) (.views *app*))
    (remove-view it))
  (let ((tracker (make-instance 'tracker
                                :tracks (.tracks (.sequencer *audio*)))))
    (append-view tracker)
    (setf (.selected-modules *app*) (list tracker))))
