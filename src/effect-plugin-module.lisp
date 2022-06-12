(in-package :colitrsynth)

(defmethod process-in ((self effect-plugin-model)
                    (connection audio-connection)
                    left right)
  (declare (optimize (speed 3) (safety 0)))
  (let ((out (.out-buffer self)))
    (declare ((simple-array (unsigned-byte 8) (*)) out in))
    (loop with j fixnum = (1- (the fixnum
                                   (* (the fixnum
                                           (* (the fixnum (.dest-bus connection))
                                              (the fixnum *frames-per-buffer*)))
                                      4 2)))
          for lr in (list left right)
          do (loop for i fixnum below *frames-per-buffer*
                   for n = (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                             (ieee-floats:encode-float32
                              (the double-float
                                   (aref (the (simple-array double-float (*)) lr) i))))
                   do (setf (aref out (incf j))
                            (ldb (byte 8 0) n))
                      (setf (aref out (incf j)) (ldb (byte 8 8) n))
                      (setf (aref out (incf j)) (ldb (byte 8 16) n))
                      (setf (aref out (incf j)) (ldb (byte 8 24) n))))))

(defmethod process-out-plugin-command ((self effect-plugin-module))
  +plugin-command-effect+)


