(in-package :colitrsynth)

(defun clear-array (array value)
  (loop for i below (length array)
        do (setf (aref array i) value)))

(defun compute-expt (value delta &optional (k 1000.0))
  (let ((x (+ (sqrt (* value k)) delta)))
    (values (/ (expt x 2) k) x)))

(defun compute-linear (value delta &optional (k 200.0))
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
                      (initial-element 0.0)
                      (element-type 'single-float))
  (make-array length :initial-element initial-element :element-type element-type))

(defun radian (x1 y1 x2 y2)
  (atan (- y2 y1) (- x2 x1)))

(defmacro with-benchmark (&body body)
  (let ((time (gensym)))
    `(let ((,time (get-internal-real-time)))
       (prog1
           (progn ,@body)
         (format t "~&with-benchmark ~fs"
                 (/ (- (get-internal-real-time) ,time)
                    internal-time-units-per-second))))))

(defmacro with-serialize-context ((out &optional serialize-table) &body body)
  `(with-standard-io-syntax
     (let ((*package* (find-package :colitrsynth))
           (*serialize-table* ,serialize-table))
       (with-output-to-string (,out)
         ,@body))))
