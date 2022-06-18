(in-package :colitrsynth)

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
          (.height pattern-editor) (- (.height self) (+ 15 *font-size*)))))

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

(defmethod keydown ((self pattern-module) value scancode mod-value)
  (unless (keydown (.pattern-editor self) value scancode mod-value)
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
