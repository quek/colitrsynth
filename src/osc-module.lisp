(in-package :colitrsynth)

(defmethod initialize-instance :after ((self osc-module-mixin) &key)
  (let ((value-text (make-instance 'label
                                   :x 25 :y 25
                                   :value (let ((f (interval-upadate-value
                                                    (lambda () (.value self))
                                                    0.3)))
                                            (lambda ()
                                              (format nil "~,5f" (funcall f)))))))
    (add-child self value-text)))

(defmethod available-connections (src (dest osc-module-mixin) cable-src)
  (list (make-instance 'midi-connection :src src :dest dest)))

(defmethod cable-buffer ((module osc-module-mixin) (connection audio-connection))
  (values (.buffer module)
          nil))

(defmethod process-out ((self osc))
  (flet ((midi-event (i on-or-off)
           (loop for x in (.midi-events self)
                   thereis (and (= (.event x) on-or-off)
                                (= (.frame x) (mod (+ (.midi-frame self) i) *frames-per-buffer*))
                                x))))
    (loop for i below *frames-per-buffer*
          for on-event = (midi-event i +midi-event-on+)
          for off-event = (midi-event i +midi-event-off+)
          for value = (cond (on-event
                             (setf (.phase self) 0
                                   (.note self) (.note on-event))
                             (osc-frame-value self))
                            (off-event
                             (setf (.note self) off)
                             0.0)
                            (t
                             (if (= off (.note self))
                                 0.0
                                 (osc-frame-value self))))
          do (setf (aref (.buffer self) i) value)
             (setf (.value self) value)))
  (route self (.buffer self) (.buffer self)))

