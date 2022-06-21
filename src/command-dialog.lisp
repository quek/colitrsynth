(in-package :colitrsynth)

(defmethod initialize-instance :after ((self command-dialog) &key)
  (setf (.reader self) (lambda ()
                         (.command self)))
  (setf (.writer self) (lambda (value)
                         (setf (.command self) value)))
  (setf (.focused-view *app*) self))

;;; TODO menu-view と共通化
(defmethod close ((self command-dialog) &key abort)
  (declare (ignore abort))
  (setf (.selected-modules *app*) (delete self (.selected-modules *app*)))
  (remove-view self)
  (call-next-method))

(defmethod lost-focuse ((self command-dialog))
  (call-next-method)
  (print (.command self))
  (close self))

