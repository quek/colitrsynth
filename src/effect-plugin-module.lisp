(in-package :colitrsynth)

(defmethod process-in ((self effect-plugin-model)
                       (connection audio-connection)
                       left right)
  (declare (optimize (speed 3) (safety 0)))
  (let ((buffer (.mix-buffer self)))
    (declare ((simple-array double-float (*)) buffer left right))
    (loop with j fixnum = (1- (the fixnum
                                   (* (the fixnum
                                           (* (the fixnum (.dest-bus connection))
                                              (the fixnum *frames-per-buffer*)))
                                      2)))
          for lr in (list left right)
          do (loop for i fixnum below *frames-per-buffer*
                   for value double-float = (aref (the (simple-array double-float (*)) lr) i)
                   do (incf (aref buffer (incf j)) value)))))

(defmethod process-out ((self effect-plugin-model))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((mix (.mix-buffer self))
         (out (.out-buffer self)))
    (declare ((simple-array double-float (*)) mix)
             ((simple-array (unsigned-byte 8) (*)) out))
    (loop with i fixnum = -1
          for f double-float across mix
          for n = (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                    (ieee-floats:encode-float32 f))
          do (setf (aref out (incf i)) (ldb (byte 8 0) n))
             (setf (aref out (incf i)) (ldb (byte 8 8) n))
             (setf (aref out (incf i)) (ldb (byte 8 16) n))
             (setf (aref out (incf i)) (ldb (byte 8 24) n)))
    (call-next-method)
    (clear-array mix 0.0d0)))

(defmethod process-out-plugin-command ((self effect-plugin-module))
  +plugin-command-effect+)

(defmethod (setf .input-nbuses) :after (value (self effect-plugin-module))
  (setf (.mix-buffer self) (make-buffer :length (* *frames-per-buffer* 2 value))))
