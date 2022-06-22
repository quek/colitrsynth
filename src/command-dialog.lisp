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
  (close self)
  (when (string/= "" (.command self))
    (let ((*package* (find-package :cmd)))
      (destructuring-bind (command &rest args) (read-from-string (format nil "(~a)" (.command self)))
        (when (and (fboundp command)
                   (get command :interactive))
          (loop for x in (.targets self)
                do (handler-case (apply command x args)
                     (sb-pcl::no-applicable-method-error ()))))))
    (setf (.selected-modules *app*) (.targets self))))

