(in-package :colitrsynth)

(defun compute-linear (value delta &optional (k 200.0d0))
  (let ((x (+ (* value k) delta)))
    (/ x k)))

(defun compute-expt (value delta &optional (k 5000.0d0))
  (let ((x (+ (sqrt (* value k)) delta)))
    (/ (expt x 2) k)))

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


