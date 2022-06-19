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

(declaim (ftype (function (fixnum fixnum fixnum fixnum)
                          (values single-float  &optional)) distance))
(declaim (inline distance))
(defun distance (x1 y1 x2 y2)
  (declare (optimize (speed 3) (safety 0))
           (fixnum x1 y1 x2 y2))
  (sqrt
   (the fixnum
        (+ (the fixnum (expt (- x2 x1) 2))
                  (the fixnum (expt (- y2 y1) 2))))))

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

(declaim (ftype (function (fixnum fixnum fixnum fixnum)
                          (values single-float  &optional)) radian))
(declaim (inline radian))
(defun radian (x1 y1 x2 y2)
  (declare (optimize (speed 3) (safety 0))
           (fixnum x1 y1 x2 y2))
  (atan (the fixnum (- y2 y1))
        (the fixnum (- x2 x1))))

(declaim (inline read-fd))
(defun read-fd (fd buffer buffer-size)
  (declare (optimize (speed 3) (safety 0)))
  (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (cffi:with-foreign-object (bytes-read '(:pointer :unsigned-long))
      (sb-sys:with-pinned-objects (buffer)
        (sb-win32:read-file fd
                            (sb-sys:vector-sap buffer)
                            buffer-size
                            bytes-read
                            (cffi:null-pointer))
        ;; (cffi:mem-aref bytes-read :unsigned-long 0)
        ;; これ動かない
        ;; (sb-posix:read (sb-sys:fd-stream-fd io)
        ;;                (sb-sys:vector-sap xxx)
        ;;                (* *frames-per-buffer* 4))
        ))))

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
