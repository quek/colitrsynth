(in-package :colitrsynth)

(defmethod initialize-instance :after ((self midi-input-module) &key device-name)
  (when device-name
    (open-midi-input self)))

(defmethod close ((self midi-input-module) &key abort)
  (declare (ignore abort))
  (alexandria:when-let ((handle (.handle self)))
    (colitrsynth.ffi::midi-in-stop handle)
    (colitrsynth.ffi::midi-in-close handle))
  (call-next-method))

(defmethod open-midi-input ((self midi-input-module))
  (multiple-value-bind (handle mailbox)
      (colitrsynth.ffi::open-midi-input (.device-name self))
    (setf (.handle self) handle)
    (setf (.mailbox self) mailbox)
    (colitrsynth.ffi::midi-in-start handle)))

(defmethod process-out ((self midi-input-module))
  (print (sb-concurrency:receive-pending-messages (.mailbox self))))
