(in-package :colitrsynth)

(defmethod initialize-instance :after ((self midi-input-module) &key device-name)
  (when device-name
    (open-midi-input self)))

(defmethod available-cables-src ((self midi-input-module))
  (list (make-instance 'midi-connection
                       :src self :dest nil)))

(defmethod close ((self midi-input-module) &key abort)
  (declare (ignore abort))
  (alexandria:when-let ((handle (.handle self)))
    (colitrsynth.ffi::close-midi-input handle))
  (call-next-method))

(defmethod open-midi-input ((self midi-input-module))
  (multiple-value-bind (handle mailbox)
      (colitrsynth.ffi::open-midi-input (.device-name self))
    (setf (.open-time self) (get-internal-real-time))
    (setf (.handle self) handle)
    (setf (.mailbox self) mailbox)))

(defmethod process-out ((self midi-input-module))
  (let ((per-frame (* (sec-per-frame) internal-time-units-per-second))
        (elapsed-time (- (get-internal-real-time) (.open-time self)
                         (* (sec-per-frame) *frames-per-buffer* internal-time-units-per-second))))
    (setf (.output-midi-events self)
          (sb-concurrency:receive-pending-messages (.mailbox self)))
    (mapc (lambda (midi-event)
            (let ((time (round (/ (max 0 (- (* (.frame midi-event) 1000)
                                            elapsed-time))
                                  per-frame))))
              (setf (.frame midi-event) time)))
          (.output-midi-events self))
    (route self nil nil)))
