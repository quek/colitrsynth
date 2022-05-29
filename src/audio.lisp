(in-package :colitrsynth)

;;(portaudio::print-devices)

(cffi:defbitfield (pa-stream-callback-flags :unsigned-long)
  (:input-underflow #x00000001)
  (:input-overflow #x00000002)
  (:output-underflow #x00000004)
  (:output-overflow #x00000008)
  (:priming-output #x00000010))

(cffi:defcstruct pa-stream-callback-time-info
  (input-buffer-adc-time pa::pa-time)
  (current-time pa::pa-time)
  (output-buffer-dac-time pa::pa-time))

(defparameter *frames-per-buffer* 1024)
(defparameter *sample-rate* 48000.0d0)
(defun sec-per-line ()
  (/ 60.0d0 (.bpm (.sequencer *audio*)) (.lpb (.sequencer *audio*))))
(defun sec-per-frame ()
  (/ 1.0d0 *sample-rate*))
(defun frames-per-line ()
  (/ (sec-per-line) (sec-per-frame)))

(defun make-buffer (&key (initial-element 0.0d0) (element-type 'double-float))
  (make-array *frames-per-buffer* :initial-element initial-element :element-type element-type))

(defclass audio ()
  ((device-api
    :initarg :device-api
    :initform "ASIO"
    :accessor .device-api)
   (device-name
    :initarg :device-name
    ;; :initform "Prism Sound USB Audio Class 2.0"
    :initform "FL Studio ASIO"
    :accessor .device-name)
   (sample-rate
    :initarg :sample-rate
    :initform *sample-rate*
    :type double-float
    :accessor .sample-rate)
   (frames-per-buffer
    :initarg frames-per-buffer
    :initform *frames-per-buffer*
    :accessor .frames-per-buffer)
   (sample-format
    :initarg sample-format
    :initform :float
    :accessor .sample-format)
   (playing :initform nil :accessor .playing)
   (request-stop :initform nil :accessor .request-stop)
   (start-time
    :initform 0.0d0
    :accessor .start-time
    :type double-float)
   (current-time :initform 0.0d0 :type double-float :accessor .current-time)
   (nframes :initform 0 :type fixnum :accessor .nframes)
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
    :accessor .output-channels)
   (buffer :accessor .buffer)
   (sequencer :initarg :sequencer :accessor .sequencer)
   (master :accessor .master)))

(defun play ()
  (unless (.playing *audio*)
    (setf (.playing *audio*) t)
    (setf (.start-time *audio*) 0.0d0)
    (setf (.request-stop *audio*) nil)
    (setf (.nframes *audio*) 0)
    (pa:start-stream (.stream *audio*))))

(defun stop ()
  (when (.playing *audio*)
    (setf (.playing *audio*) nil)
    (pa::stop-stream (.stream *audio*))))

(defun request-stop ()
  (setf (.request-stop *audio*) t))


(defun line-and-frame ()
  (let* ((sec-per-line (sec-per-line))
         (sec-per-frame (sec-per-frame))
         (current-sec (* sec-per-frame (.nframes *audio*)))
         (start-line (floor (/ current-sec sec-per-line)))
         (start-frame (floor (/ (mod current-sec sec-per-line) sec-per-frame)))
         (end-line start-line)
         (end-frame (+ start-frame *frames-per-buffer*)))
    (when (< sec-per-line (+ (* sec-per-frame start-frame)
                           (* sec-per-frame *frames-per-buffer*)))
      (incf end-line)
      (decf end-frame (floor (/ sec-per-line sec-per-frame))))
    (values start-line start-frame end-line end-frame)))

(defun write-master-buffer ()
  (flet ((limit (value)
           (coerce
            (cond ((< 1.0d0 value)
                   (warn "音大きすぎ ~a" value)
                   1.0d0)
                  ((< value -1.0d0)
                   (warn "音大きすぎ ~a" value)
                   -1.0d0)
                  (t value))
            'single-float)))
    (let* ((master (.master *audio*))
           (volume (.volume master)))
      (loop for i below *frames-per-buffer*
            do (setf (cffi:mem-aref (.buffer *audio*) :float (* i 2))
                     (limit (* (aref (.left master) i) volume))
                     (cffi:mem-aref (.buffer *audio*) :float (1+ (* i 2)))
                     (limit (* (aref (.right master) i) volume))
                     (aref (.left master) i) 0.0d0
                     (aref (.right master) i) 0.0d0)))))

(cffi:defcallback audio-callback :int ((input-buffer :pointer)
                                       (output-buffer :pointer)
                                       (frame-per-buffer :unsigned-long)
                                       (time-info (:pointer (:struct pa-stream-callback-time-info)))
                                       (status-flags pa-stream-callback-flags)
                                       (user-data :pointer))
  (declare ;; (optimize (speed 3) (safety 0))
   (ignore input-buffer status-flags user-data))
  (assert (= frame-per-buffer *frames-per-buffer*))
  (let ((current-time (cffi:foreign-slot-value time-info '(:struct pa-stream-callback-time-info)
                                               'current-time)))
    (declare (double-float current-time))
    (when (= (the double-float (.start-time *audio*)) 0.0d0)
      (setf (.start-time *audio*) current-time))
    (setf (.current-time *audio*) (the double-float (- current-time (the double-float (.start-time *audio*)))))
    (setf (.buffer *audio*) output-buffer))

  (multiple-value-bind (start-line start-frame end-line end-frame) (line-and-frame)
    (play-sequencer (.sequencer *audio*) start-line start-frame end-line end-frame))
  (incf (.nframes *audio*) frame-per-buffer)
  0)

(defmacro with-audio (&body body)
  `(let ((*audio* (setf *audio* (make-instance 'audio))))
     (portaudio:with-audio
       (cffi:with-foreign-objects ((handle :pointer))
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
                (setf (.stream *audio*)
                      (progn
                        (pa::raise-if-error
                         (pa::%open-stream
                          handle
                          nil
                          output-parameters
                          *sample-rate*
                          (.frames-per-buffer *audio*)
                          0
                          (cffi:callback audio-callback)
                          (cffi:null-pointer)))
                        (make-instance
                         'pa:pa-stream
                         :handle (cffi:mem-ref handle :pointer)
                         :input-sample-format (.sample-format *audio*)
                         :input-channels (if (zerop (the fixnum (.input-channels *audio*))) nil (.input-channels *audio*))
                         :output-sample-format (.sample-format *audio*)
                         :output-channels (if (zerop (the fixnum (.output-channels *audio*))) nil (.output-channels *audio*))
                         :frames-per-buffer (.frames-per-buffer *audio*))))
                ,@body)
           (when (.stream *audio*)
             (stop)
             (pa:close-stream (.stream *audio*))))))))

(defmacro delegate-model (class)
  `(progn
     ,@(loop for slot in (sb-mop:class-slots (find-class 'model))
             for name = (intern (format nil ".~a"
                                        (sb-mop:slot-definition-name slot)))
             nconc `((defmethod ,name ((self ,class))
                       (,name (.model self)))
                     (defmethod (setf ,name) (value (self ,class))
                       (setf (,name (.model self)) value))))))

