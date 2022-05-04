(in-package :colitrsynth.audio)

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
   (buffer-index :initform 0 :accessor .buffer-index :type fixnum)
   (bpm :initarg :bpm :initform 90.0d0 :accessor .bpm
        :type double-float)
   (lpb :initarg :lpb :initform 4 :accessor .lpb)
   (sequencer :initarg :sequencer :accessor .sequencer
              :initform (make-instance 'sequencer))
   (out :initform (make-instance 'out) :accessor .out)))

(defun line-and-frame ()
  (let* ((sec-per-line (/ 60.0d0 (.bpm *audio*) (.lpb *audio*)))
         (sec-per-frame (/ 1.0d0 (.sample-rate *audio*)))
         (current-sec (* sec-per-frame (.nframes *audio*))))
    (multiple-value-bind (line remain) (floor (/ current-sec sec-per-line))
      (values line (floor (/ remain sec-per-frame))))))

(defun push-to-buffer (value)
  (setf (cffi:mem-aref (.buffer *audio*) :float (.buffer-index *audio*))
        (coerce value 'single-float))
  (incf (.buffer-index *audio*)))

(defclass sequencer ()
  ((patterns :initarg :patterns :accessor .patterns
             :initform nil)
   (end :initform 0 :accessor .end)))

(defmethod add-pattern ((self sequencer) (pattern pattern) line)
  (setf (.end self) (max (.end self)
                         (+ line (length (.lines pattern)))))
  (push (cons line pattern) (.patterns self)))

(defun play-sequencer (self line frame)
  (declare (ignore frame))
  (if (< (.end self) line)
      (progn
        (setf (.playing *audio*) nil)
       (loop for i below (.frames-per-buffer *audio*)
             do (push-to-buffer 0)      ;left
                (push-to-buffer 0)))      ;right
      (loop for i below (.frames-per-buffer *audio*)
            with out = (.out *audio*)
            do (loop for (start . pattern) in (.patterns self)
                     if (<= start line (+ start (length (.lines pattern))))
                       do (play-pattern pattern line))
               (let ((value (* (.value out) (.volume out))))
                 (push-to-buffer value) ;left
                 (push-to-buffer value)) ;right
               (setf (.value out) 0.0d0))))

(defclass module ()
  ((children :initarg :children :accessor .children :initform nil)))

(defclass pattern (module)
  ((lines :initarg :lines
          :initform (list c4 d4 e4 f4) :accessor .lines)
   (last-note :initform off :accessor .last-note)))

(defun play-pattern (self line)
  ;; TODO
  (let* ((note (nth (mod line (length (.lines self))) (.lines self)))
         (note (if (= note none)
                   (.last-note self)
                   note)))
    (loop for child in (.children self)
          do (%play child note))
    (setf (.last-note self) note)))

(defclass sin-wave (module)
  ((note :initarg :note :initform off :accessor .note)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod %play ((self sin-wave) note)
  (let ((value
          (if (= note off)
              0.0d0
              (progn
                (when (/= (.note self) note)
                  (setf (.phase self) 0))
                (setf (.note self) note)
                (sin (* (/ (* 2 pi (midino-to-freq note)) (.sample-rate *audio*))
                         (.phase self)))))))
    (loop for child in (.children self)
          do (%play child value)))
  (incf (.phase self)))

(defclass adsr (module)
  ((a :initarg :a :initform 0.003d0 :accessor .a)
   (d :initarg :d :initform 0.05d0 :accessor .d)
   (s :initarg :s :initform 0.3d0 :accessor .s)
   (r :initarg :r :initform 0.1d0 :accessor .r)
   (frame :initform 0 :accessor .frame)
   (last-value :initform 0.0d0 :accessor .last-value)
   (release-time :initform nil :accessor .release-time)
   (release-value :initform nil :accessor .release-value)))

(defmethod %play ((self adsr) value)
  (let* ((sec-per-frame (/ 1.0d0 (.sample-rate *audio*)))
         (current (* sec-per-frame (.frame self)))
         (value (if (= value 0.0d0)
                    (progn
                      (when (null (.release-time self))
                        (setf (.release-time self) current
                              (.release-value self) (.last-value self)))
                      (let ((elapsed (- current (.release-time self))))
                        (if (< elapsed (.r self))
                            (- (.release-value self)
                               (* (/ (.release-value self)
                                     (.r self))
                                  elapsed))
                            0.0d0)))
                    (progn
                      (setf (.release-time self) nil)
                      (cond ((< current (.a self))
                             (* value (/ 1.0d0 (.a self)) current))
                            ((< current (+ (.a self) (.d self)))
                             (* value (- 1.0d0 (* (/ (- 1.0d0 (.s self)) (.d self))
                                                  (- current (.a self))))))
                            (t (* value (.s self))))))))
    (loop for child in (.children self)
          do (%play child value))
    (incf (.frame self))
    (setf (.last-value self) value)))

(defclass out ()
  ((value :initform 0.0d0 :accessor .value)
   (volume :initform 0.6d0 :accessor .volume)))

(defmethod %play ((self out) value)
  (incf (.value self) value))

(defun proc (input-buffer
             output-buffer
             frame-per-buffer
             time-info
             status-flags)
  (declare ;; (optimize (speed 3) (safety 0))
           (ignore input-buffer status-flags))
  (let ((current-time (cffi:foreign-slot-value time-info '(:struct pa-stream-callback-time-info)
                                               'current-time)))
    (declare (double-float current-time))
    (when (= (the double-float (.start-time *audio*)) 0.0d0)
      (setf (.start-time *audio*) current-time))
    (setf (.current-time *audio*) (the double-float (- current-time (the double-float (.start-time *audio*)))))
    (setf (.buffer *audio*) output-buffer)
    (setf (.buffer-index *audio*) 0))

  (multiple-value-bind (line frame) (line-and-frame)
    (play-sequencer (.sequencer *audio*) line frame))
  (incf (.nframes *audio*) frame-per-buffer)
  0)

(cffi:defcallback my-callback :int ((input-buffer :pointer)
                                    (output-buffer :pointer)
                                    (frame-per-buffer :unsigned-long)
                                    (time-info (:pointer (:struct pa-stream-callback-time-info)))
                                    (status-flags pa-stream-callback-flags)
                                    (user-data :pointer))
  (declare (ignore user-data))
  (funcall 'proc
           input-buffer
           output-buffer
           frame-per-buffer
           time-info
           status-flags))

(defun start-audio ()
  ;;(declare (optimize (speed 3) (safety 0)))
  (let* ((*audio* (setf *audio* (make-instance 'audio)))
         (pattern1 (make-instance
                    'pattern
                    :lines (list a4 e4 none g4
                                 a4 off  g4 c4)
                    :children (list (make-instance
                                     'sin-wave
                                     :children (list (make-instance
                                                      'adsr
                                                      :children (list (.out *audio*))))))))
         (pattern2 (make-instance
                    'pattern
                    :lines (list c4 g4 none b4
                                 c4 off  b4 d4)
                    :children (list (make-instance
                                     'sin-wave
                                     :children (list (make-instance
                                                      'adsr
                                                      :children (list (.out *audio*)))))))))
    (add-pattern (.sequencer *audio*) pattern1 0)
    (add-pattern (.sequencer *audio*) pattern1 (length (.lines pattern1)))
    (add-pattern (.sequencer *audio*) pattern2 (length (.lines pattern1)))
    (add-pattern (.sequencer *audio*) pattern1 (* 2 (length (.lines pattern1))))
    (add-pattern (.sequencer *audio*) pattern2 (* 2 (length (.lines pattern1))))
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
                         (.sample-rate *audio*)
                         (.frames-per-buffer *audio*)
                         0
                         (cffi:callback my-callback)
                         (cffi:null-pointer)))
                       (make-instance
                        'pa:pa-stream
                        :handle (cffi:mem-ref handle :pointer)
                        :input-sample-format (.sample-format *audio*)
                        :input-channels (if (zerop (the fixnum (.input-channels *audio*))) nil (.input-channels *audio*))
                        :output-sample-format (.sample-format *audio*)
                        :output-channels (if (zerop (the fixnum (.output-channels *audio*))) nil (.output-channels *audio*))
                        :frames-per-buffer (.frames-per-buffer *audio*)))))
          (setf (.playing *audio*) t)
          (setf (.start-time *audio*) 0.0d0)
          (pa:start-stream (.stream *audio*))
          (loop while (.playing *audio*) do (pa:pa-sleep 10)))
        (when (.stream *audio*)
          (pa::stop-stream (.stream *audio*))
          (pa:close-stream (.stream *audio*)))))))
