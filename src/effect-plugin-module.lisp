(in-package :colitrsynth)

(defmethod available-connections (src (dest effect-plugin-model) (cable-src audio-connection))
  (append
   (loop for i below (.input-nbuses dest)
         collect (make-instance 'audio-connection
                                :src src :dest dest
                                :src-bus (.src-bus cable-src)
                                :dest-bus i))
   (call-next-method)))

(defmethod get-params :after ((self effect-plugin-model))
  (setf (.out-buffer self)
        (make-array (.out-length self) :element-type 'single-float)))

(defmethod process-in ((self effect-plugin-model)
                       (connection audio-connection)
                       left right)
  (declare (optimize (speed 3) (safety 0)))
  (let ((buffer (.mix-buffer self)))
    (declare ((simple-array single-float (*)) buffer left right))
    (loop with j fixnum = (1- (the fixnum
                                   (* (the fixnum
                                           (* (the fixnum (.dest-bus connection))
                                              (the fixnum *frames-per-buffer*)))
                                      2)))
          for lr in (list left right)
          do (loop for i fixnum below *frames-per-buffer*
                   for value single-float = (aref (the (simple-array single-float (*)) lr) i)
                   do (incf (aref buffer (incf j)) value)))))

(defmethod process-out ((self effect-plugin-model))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((mix (.mix-buffer self))
         (out (.out-buffer self)))
    (declare ((simple-array single-float (*)) mix out))
    (loop with i fixnum = -1
          for f single-float across mix
          do (setf (aref out (incf i)) f))
    (call-next-method)
    ;; TODO cable のためにクリアのタイミング
    (clear-array mix 0.0)))

(defmethod process-out-plugin-command ((self effect-plugin-module))
  +plugin-command-effect+)

(defmethod (setf .input-nbuses) :after (value (self effect-plugin-module))
  (setf (.mix-buffer self) (make-buffer :length (* *frames-per-buffer* 2 value))))

(defmethod write-out-buffer-to-plugin ((self effect-plugin-module))
  (declare (optimize (speed 3) (safety 0)))
  (let ((out-buffer (.out-buffer self)))
    (declare ((simple-array single-float (*)) out))
    (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (cffi:with-foreign-object (bytes-written '(:pointer :unsigned-long))
        (sb-sys:with-pinned-objects (out-buffer)
          (sb-win32:write-file (sb-sys:fd-stream-fd (.host-io self))
                               (sb-sys:vector-sap out-buffer)
                               (.out-length self)
                               bytes-written
                               (cffi:null-pointer)))))))
