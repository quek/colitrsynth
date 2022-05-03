(in-package :colitrsynth)

;;(portaudio::print-devices)

(cffi:defbitfield (pa-stream-callback-flags :unsigned-long)
  (:input-underflow #x00000001)
  (:input-overflow #x00000002)
  (:output-underflow #x00000004)
  (:output-overflow #x00000008)
  (:priming-output #x00000010))

(cffi:defcstruct user-data
  (freq :float)
  (index :float))

(cffi:defcstruct pa-stream-callback-time-info
  (input-buffer-adc-time pa::pa-time)
  (current-time pa::pa-time)
  (output-buffer-dac-time pa::pa-time))

(defvar *audio* nil)

(defclass audio ()
  ((device-api
    :initarg :device-api
    :initform "ASIO"
    :accessor .device-api)
   (device-name
    :initarg :device-name
    :initform "Prism Sound USB Audio Class 2.0"
    :accessor .device-name)
   (sample-rate
    :initarg :sample-rate
    :initform 48000.0d0
    :type double-float
    :accessor .sample-rate)
   (frames-per-buffer
    :initarg frames-per-buffer
    :initform 1024
    :accessor .frames-per-buffer)
   (sample-format
    :initarg sample-format
    :initform :float
    :accessor .sample-format)
   (playing
    :initform nil
    :accessor .playing)
   (last-time
    :initform 0.0d0
    :accessor .last-time
    :type double-float)
   (stream
    :initform nil
    :accessor .stream)
   (input-channels
    :initarg :input-channels
    :initform 0
    :type fixnum
    :accessor .input-channels)
   (output-channels
    :initarg :output-channels
    :initform 2
    :type fixnum
    :accessor .output-channels)))

(defun proc (input-buffer
             output-buffer
             frame-per-buffer
             time-info
             status-flags
             user-data)
  (declare (optimize (speed 3) (safety 0))
           (ignore input-buffer status-flags))
  (let ((current-time (cffi:foreign-slot-value time-info '(:struct pa-stream-callback-time-info)
                                               'current-time)))
    (declare (double-float current-time))
    ;; (cffi:with-foreign-slots ((input-buffer-adc-time current-time output-buffer-dac-time)
    ;;                           time-info
    ;;                           (:struct pa-stream-callback-time-info))
    ;;   (format t "~&~a ~a ~a" input-buffer-adc-time current-time output-buffer-dac-time))
    (let* (
           (delta (- current-time (the double-float (.last-time *audio*))))
           (vol 0.4)
           (env (if (> delta 1.0d0)
                    1.0
                    2.0)))
      (when (> delta 2)
        (setf (.last-time *audio*) current-time))
      (loop for i of-type fixnum from 0 below frame-per-buffer
            for l of-type fixnum = (* i 2)
            for r of-type fixnum = (1+ l)
            for sin-arg = (the double-float
                               (/ (the double-float
                                       (* 2 pi env
                                          (cffi:foreign-slot-value user-data '(:struct user-data) 'freq)
                                          (cffi:foreign-slot-value user-data '(:struct user-data) 'index)))
                                  (the double-float *sample-rate*)))
            
            do (setf (cffi:mem-aref output-buffer :float l)
                     (* (coerce (sin sin-arg) 'single-float)
                        vol)
                     (cffi:mem-aref output-buffer :float r)
                     (* (coerce (sin (* sin-arg 2)) 'single-float)
                        vol))
               (incf (cffi:foreign-slot-value user-data '(:struct user-data) 'index) 1.0))))
  0)

(cffi:defcallback my-callback :int ((input-buffer :pointer)
                                    (output-buffer :pointer)
                                    (frame-per-buffer :unsigned-long)
                                    (time-info (:pointer (:struct pa-stream-callback-time-info)))
                                    (status-flags pa-stream-callback-flags)
                                    (user-data :pointer))
  (funcall 'proc
           input-buffer
           output-buffer
           frame-per-buffer
           time-info
           status-flags
           user-data))

(defun callback-test ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((*audio* (setf *audio* (make-instance 'audio))))
    (portaudio:with-audio
      (cffi:with-foreign-objects ((handle :pointer)
                                  ;; たぶんいらない
                                  (user-data '(:struct user-data)))
        (unwind-protect
             (let* ((output-parameters (pa::make-stream-parameters)))
               (setf (pa:stream-parameters-channel-count output-parameters) (.output-channels *audio*)
                     (pa:stream-parameters-sample-format output-parameters) (.sample-format *audio*)
                     (pa::stream-parameters-suggested-latency output-parameters) 0.0d0) ;TODO
               (loop for i of-type fixnum below (pa:get-device-count)
                     for device-info = (pa:get-device-info i)
                     if (and
                         (equal (pa:host-api-info-name
                                 (pa:get-host-api-info (pa:device-info-host-api device-info)))
                                (.device-api *audio*))
                         (equal (pa:device-info-name device-info) (.device-name *audio*)))
                       do (setf (pa::stream-parameters-device output-parameters) i)
                          (loop-finish))
               
               (setf (cffi:foreign-slot-value user-data '(:struct user-data)
                                              'freq)
                     440.0
                     (cffi:foreign-slot-value user-data '(:struct user-data)
                                              'index)
                     0.0)
               (setf (.stream *audio*)
                     (progn
                       (pa::raise-if-error
                        (pa::%open-stream
                         handle
                         nil
                         output-parameters
                         (.sample-rate *audio*)
                         (.frames-per-buffer *audio*)
                         0
                         (cffi:callback my-callback)
                         user-data))
                       (make-instance
                        'pa:pa-stream
                        :handle (cffi:mem-ref handle :pointer)
                        :input-sample-format (.sample-format *audio*)
                        :input-channels (if (zerop (the fixnum (.input-channels *audio*))) nil (.input-channels *audio*))
                        :output-sample-format (.sample-format *audio*)
                        :output-channels (if (zerop (the fixnum (.output-channels *audio*))) nil (.output-channels *audio*))
                        :frames-per-buffer (.frames-per-buffer *audio*)))))
          (setf (.playing *audio*) t)
          (setf (.last-time *audio*) 0.0d0)
          (pa:start-stream (.stream *audio*))
          ;; (loop while *playing* do (pa:pa-sleep 100))
          (pa:pa-sleep 3000))
        (when (.stream *audio*)
          (pa::stop-stream (.stream *audio*))
          (pa:close-stream (.stream *audio*)))))))
