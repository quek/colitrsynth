(in-package :colitrsynth)

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
