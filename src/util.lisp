(in-package :colitrsynth)

(defun clear-array (array value)
  (loop for i below (length array)
        do (setf (aref array i) value)))

(defun compute-expt (value delta &optional (k 1000.0d0))
  (let ((x (+ (sqrt (* value k)) delta)))
    (values (/ (expt x 2) k) x)))

(defun compute-linear (value delta &optional (k 200.0d0))
  (let ((x (+ (* value k) delta)))
    (values (/ x k) x)))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

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

(defun make-buffer (&key (length *frames-per-buffer*)
                      (initial-element 0.0d0)
                      (element-type 'double-float))
  (make-array length :initial-element initial-element :element-type element-type))

(defun radian (x1 y1 x2 y2)
  (atan (- y2 y1) (- x2 x1)))
