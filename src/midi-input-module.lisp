(in-package :colitrsynth)

(defmethod initialize-instance :after ((self midi-input-module) &key device-name)
  (when device-name
    (open-midi-input self)))

(defmethod close ((self midi-input-module) &key abort)
  (declare (ignore abort))
  (alexandria:when-let ((handle (.handle self)))
    (colitrsynth.ffi::close-midi-input handle))
  (call-next-method))

(defmethod open-midi-input ((self midi-input-module))
  (multiple-value-bind (handle mailbox)
      (colitrsynth.ffi::open-midi-input (.device-name self))
    (setf (.handle self) handle)
    (setf (.mailbox self) mailbox)))

(defmethod process-out ((self midi-input-module))
  (print (sb-concurrency:receive-pending-messages (.mailbox self))))
