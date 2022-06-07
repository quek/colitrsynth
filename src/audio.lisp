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
    :initform "Prism Sound USB Audio Class 2.0"
    ;; :initform "FL Studio ASIO"
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
   (processing :initform nil :accessor .processing)
   (playing :initform nil :accessor .playing)
   (played :initform nil :accessor .played)
   (request-stop :initform nil :accessor .request-stop)
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

(defun start-audio ()
  (unless (.processing *audio*)
    (setf (.processing *audio*) t)
    (setf (.request-stop *audio*) nil)
    (pa:start-stream (.stream *audio*))))

(defun stop-audio ()
  (when (.processing *audio*)
    (setf (.processing *audio*) nil)
    (pa::stop-stream (.stream *audio*))))

(defun play-from-start ()
  (%play (make-play-position :line 0 :line-frame 0)))

(defun play-from-last ()
  (%play (.last-play-position (.sequencer *audio*))))

(defun play-from-current ()
  (let* ((sequencer (.sequencer *audio*)))
    (setf (.last-play-position sequencer) (.play-position sequencer))
    (%play (.play-position sequencer))))

(defun %play (play-position)
  (let* ((sequencer (.sequencer *audio*)))
    (setf (.play-position sequencer) play-position))
  (setf (.playing *audio*) t))

(defun stop ()
  (setf (.playing *audio*) nil))

(defun playing ()
  (.playing *audio*))

(defun played ()
  (.played *audio*))

(defun request-stop ()
  (setf (.request-stop *audio*) t))

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
           (volume (.volume master))
           (last-left 0.0d0)
           (last-right 0.0d0))
      (loop for i below *frames-per-buffer*
            do (setf last-left
                     (max (abs (setf (cffi:mem-aref (.buffer *audio*) :float (* i 2))
                                     (limit (* (aref (.left master) i) volume))))
                          last-left))
               (setf last-right
                     (max (abs (setf  (cffi:mem-aref (.buffer *audio*) :float (1+ (* i 2)))
                                      (limit (* (aref (.right master) i) volume))))
                          last-right))
               (setf (aref (.left master) i) 0.0d0
                     (aref (.right master) i) 0.0d0))
      (setf (.last-left master) last-left)
      (setf (.last-right master) last-right))))

(cffi:defcallback audio-callback :int ((input-buffer :pointer)
                                       (output-buffer :pointer)
                                       (frame-per-buffer :unsigned-long)
                                       (time-info (:pointer (:struct pa-stream-callback-time-info)))
                                       (status-flags pa-stream-callback-flags)
                                       (user-data :pointer))
  (declare ;; (optimize (speed 3) (safety 0))
   (ignore input-buffer time-info status-flags user-data))
  (assert (= frame-per-buffer *frames-per-buffer*))
  (setf (.buffer *audio*) output-buffer)
  (let* ((sequencer (.sequencer *audio*))
         (start-play-position (.play-position sequencer))
         (end-play-position (inc-frame start-play-position))
         (looped (and (.looping sequencer) (<= (.end sequencer) (play-position-line end-play-position))))
         (playing (.playing *audio*)))
    (when looped
      (setf (play-position-line end-play-position) 0))
    (process-sequencer sequencer
                       (play-position-line start-play-position)
                       (play-position-line-frame start-play-position)
                       (play-position-line end-play-position)
                       (play-position-line-frame end-play-position))
    (when playing
      (setf (.play-position sequencer) end-play-position))
    (setf (.played *audio*) playing))
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
             (stop-audio)
             (pa:close-stream (.stream *audio*))))))))

