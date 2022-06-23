(in-package :colitrsynth)

(defmethod get-params :after ((self instrument-plugin-model))
  (setf (.out-buffer self)
        (make-array (.out-length self) :element-type '(unsigned-byte 8))))

(defmethod available-connections (src (dest instrument-plugin-model) cable-src)
  (cons (make-instance 'midi-connection :src src :dest dest)
        (call-next-method)))

(defmethod process-out ((self instrument-plugin-model))
  (let* ((midi-events (sort (.midi-events self)
                            (lambda (a b)
                              (cond ((/= (.frame a) (.frame b))
                                     (< (.frame a) (.frame b)))
                                    ((/= (.event a) (.event b))
                                     (< (.event a) (.event b)))
                                    (t (< (.note a) (.note b)))))))
         (i -1)         
         (length (length (the list midi-events)))
         (out (.out-buffer self)))
    (declare ((simple-array (unsigned-byte 8) (*)) out))
    (setf (aref out (incf i)) (mod length #x100))
    (setf (aref out (incf i)) (mod (ash length -8) #x100))
    (loop for midi-event in midi-events
          do (setf (aref out (incf i)) (.event midi-event))
             (setf (aref out (incf i)) (.channel midi-event))
             (setf (aref out (incf i)) (.note midi-event))
             (setf (aref out (incf i)) (.velocity midi-event))
             (setf (aref out (incf i)) (mod (the fixnum (.frame midi-event))
                                            #x100))
             (setf (aref out (incf i)) (mod (ash
                                             (the fixnum (.frame midi-event))
                                             -8) #x100)))
    (setf (.out-length self) (1+ i))
    (call-next-method)))

(defmethod process-out-plugin-command ((self instrument-plugin-module))
  +plugin-command-instrument+)

(defmethod receive-from-plugin ((self instrument-plugin-module))
  (declare (optimize (speed 3) (safety 0)))
  (call-next-method)
  (when (produces-midi-p self)
    (let ((fd (sb-sys:fd-stream-fd (.host-io self)))
          (nevents 0)
          (buffer (.out-buffer self)))   ;MIDI データ用を流用
      (declare ((simple-array (unsigned-byte 8) (*)) buffer)
               ((unsigned-byte 32) fd))
      (read-fd fd buffer 2)
      (setf (ldb (byte 4 0) nevents) (aref buffer 0))
      (setf (ldb (byte 4 4) nevents) (aref buffer 1))
      (setf (.output-midi-events self)
            (when (plusp nevents)
              (read-fd fd buffer (* 6 nevents))
              (loop repeat nevents
                    with index fixnum = -1
                    collect (let ((event (aref buffer (incf index)))
                                  (channel (aref buffer (incf index)))
                                  (note (aref buffer (incf index)))
                                  (velocity (aref buffer (incf index)))
                                  (frame 0))
                              (setf (ldb (byte 4 0) frame) (aref buffer (incf index)))
                              (setf (ldb (byte 4 4) frame) (aref buffer (incf index)))
                              (make-instance 'midi-event
                                             :event event
                                             :channel channel
                                             :note note
                                             :velocity velocity
                                             :frame frame))))))))

(defmethod write-out-buffer-to-plugin ((self instrument-plugin-module))
  (declare (optimize (speed 3) (safety 0)))
  (let ((out-buffer (.out-buffer self)))
    (declare ((simple-array (unsigned-byte 8) (*)) out))
    (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (cffi:with-foreign-object (bytes-written '(:pointer :unsigned-long))
        (sb-sys:with-pinned-objects (out-buffer)
          (sb-win32:write-file (sb-sys:fd-stream-fd (.host-io self))
                               (sb-sys:vector-sap out-buffer)
                               (.out-length self)
                               bytes-written
                               (cffi:null-pointer)))))))
