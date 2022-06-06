(in-package :colitrsynth)

(defun interval-upadate-value (value-function interval)
  (let ((time (get-internal-real-time))
        (value nil))
    (lambda ()
      (when (or (not  value)
                (< (* internal-time-units-per-second interval)
                   (- (get-internal-real-time) time)))
        (setf value (funcall value-function))
        (setf time (get-internal-real-time)))
      value)))
