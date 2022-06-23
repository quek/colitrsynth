(in-package :colitrsynth)

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

(defmethod available-connections (src (dest adsr-module) cable-src)
  (list (make-instance 'midi-connection :src src :dest dest)))

(defmethod cable-buffer ((module adsr-module) (connection audio-connection))
  (values (.buffer module)
          nil))

(defmethod process-out ((self adsr))
  (flet ((midi-event (i on-or-off)
           (loop for x in (.midi-events self)
                   thereis (and (or (= (.event x) on-or-off)
                                    (and (= on-or-off +midi-event-off+)
                                         (= (.event x) +midi-cc+)
                                         (= (.note x) +midi-cc-all-notes-off+)))
                                (= (.frame x) (mod (+ (.midi-frame self) i) *frames-per-buffer*))
                                x))))
    (loop with sec-per-frame = (/ 1.0 *sample-rate*)
          for i below *frames-per-buffer*
          for off-event = (midi-event i +midi-event-off+)
          for on-event = (midi-event i +midi-event-on+) 
          for gate = (or on-event
                         (and (not off-event)
                              (.last-gate self)))
          for current = (* sec-per-frame (.frame self))
          if on-event
            do (setf (.frame self) 0)
          do (let ((value (if gate
                              (progn
                                (setf (.release-time self) nil)
                                (cond ((< current (.a self))
                                       (* (/ 1.0 (.a self)) current))
                                      ((< current (+ (.a self) (.d self)))
                                       (- 1.0 (* (/ (- 1.0 (.s self)) (.d self))
                                                   (- current (.a self)))))
                                      (t (.s self))))
                              (progn
                                (when (null (.release-time self))
                                  (setf (.release-value self) (.last-value self))
                                  (setf (.release-time self) current))
                                (let ((elapsed (- current (.release-time self))))
                                  (if (< elapsed (.r self))
                                      (max (* (.release-value self)
                                              (- 1.0 (/ elapsed (.r self))))
                                           0.0)
                                      0.0))))))
               (setf (aref (.buffer self) i) value)
               (incf (.frame self))
               (setf (.last-gate self) gate)
               (setf (.last-value self) value))))
  (let ((buffer (.buffer self)))
    (route self buffer buffer)))

(defmethod serialize ((self adsr-module))
  `((setf (.a x) ,(.a self)
          (.d x) ,(.d self)
          (.s x) ,(.s self)
          (.r x) ,(.r self))
    ,@(call-next-method)))
