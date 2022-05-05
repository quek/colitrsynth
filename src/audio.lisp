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

(defun play-audio ()
  (pa:start-stream (.stream *audio*)))

(defun stop-audio ()
  (pa::stop-stream (.stream *audio*)))

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

(defun write-out-buffer ()
  (let* ((out (.out *audio*))
         (volume (.volume out))
         (left (* (.left out) volume))
         (right (* (.right out) volume)))
    (push-to-buffer left)
    (push-to-buffer right)
    (setf (.left out) 0.0d0
          (.right out) 0.0d0)))

(defclass sequencer ()
  ((patterns :initarg :patterns :accessor .patterns
             :initform nil)
   (end :initform 0 :accessor .end)))

(defun play-sequencer (self line frame)
  (declare (ignore frame))
  (if (< (.end self) line)
      (progn
        (setf (.playing *audio*) nil)
        (loop for i below (.frames-per-buffer *audio*)
              do (write-out-buffer)))
      (loop for i below (.frames-per-buffer *audio*)
            do (loop for (start . pattern) in (.patterns self)
                     if (<= start line (1- (+ start (length (.lines pattern)))))
                       do (play-pattern pattern (- line start)))
               (write-out-buffer))))

(defgeneric play (module left right))

(defclass module ()
  ((parents :initarg :parents :accessor .parents :initform nil)
   (children :initarg :children :accessor .children :initform nil)))

(defmethod connect ((parent module) (child module))
  (push child (.children parent))
  (push parent (.parents child)))

(defmethod play-children ((self module) left right)
  (loop for child in (.children self)
        do (play child left right)))

(defclass pattern (module)
  ((lines :initarg :lines
          :initform (list c4 d4 e4 f4) :accessor .lines)
   (last-note :initform off :accessor .last-note)))

(defmethod add-pattern ((self sequencer) (pattern pattern) line)
  (setf (.end self) (max (.end self)
                         (+ line (length (.lines pattern)))))
  (push (cons line pattern) (.patterns self)))

(defun play-pattern (self line)
  (let* ((note (nth line (.lines self)))
         (note (if (= note none)
                   (.last-note self)
                   note))
         (gate (and (/= note off)
                    ;;こんな実装でいいの？
                    (= (.last-note self) note))))
    (play-children self note gate)
    (setf (.last-note self) note)))

(defclass sin-wave (module)
  ((note :initarg :note :initform off :accessor .note)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod play ((self sin-wave) note gate)
  (declare (ignore gate))
  (let ((value
          (if (= note off)
              0.0d0
              (progn
                (when (/= (.note self) note)
                  (setf (.phase self) 0))
                (setf (.note self) note)
                (sin (* (/ (* 2 pi (midino-to-freq note)) (.sample-rate *audio*))
                        (.phase self)))))))
    (play-children self value value))
  (incf (.phase self)))

(defclass saw-wave (module)
  ((note :initarg :note :initform off :accessor .note)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod play ((self saw-wave) note gate)
  (declare (ignore gate))
  (let ((value
          (* 0.3                        ;TODO 音大きいのでとりあえずつけとく。本来はいらない？
           (if (= note off)
               0.0d0
               (progn
                 (when (/= (.note self) note)
                   (setf (.phase self) 0))
                 (setf (.note self) note)
                 (- (* (mod (/ (* (.phase self) (midino-to-freq note))
                               (.sample-rate *audio*))
                            1)
                       2)
                    1))))))
    (play-children self value value))
  (incf (.phase self)))

(defclass adsr (module)
  ((a :initarg :a :initform 0.003d0 :accessor .a)
   (d :initarg :d :initform 0.05d0 :accessor .d)
   (s :initarg :s :initform 0.3d0 :accessor .s)
   (r :initarg :r :initform 0.1d0 :accessor .r)
   (last-gate :initform nil :accessor .last-gate)
   (frame :initform 0 :accessor .frame)
   (release-time :initform nil :accessor .release-time)
   (release-value :initform nil :accessor .release-value)))

(defmethod play ((self adsr) note gate)
  (declare (ignore note))
  (when (and gate (not (.last-gate self)))
    (setf (.frame self) 0))
  (let* ((sec-per-frame (/ 1.0d0 (.sample-rate *audio*)))
         (current (* sec-per-frame (.frame self)))
         (value (if gate
                    (progn
                      (setf (.release-time self) nil)
                      (cond ((< current (.a self))
                             (* (/ 1.0d0 (.a self)) current))
                            ((< current (+ (.a self) (.d self)))
                             (- 1.0d0 (* (/ (- 1.0d0 (.s self)) (.d self))
                                         (- current (.a self)))))
                            (t (.s self))))
                    (progn
                      (when (null (.release-time self))
                        (setf (.release-time self) current))
                      (let ((elapsed (- current (.release-time self))))
                        (if (< elapsed (.r self))
                            (- 1.0d0
                               (/ elapsed (.r self)))
                            0.0d0))))))
    (play-children self value value)
    (incf (.frame self))
    (setf (.last-gate self) gate)))

(defclass amp (module)
  ((left :initform 1.0d0 :accessor .left)
   (right :initform 1.0d0 :accessor .right)
   (in-count :initform 0 :accessor .in-count)))

(defmethod play ((self amp) left right)
  (setf (.left self) (* (.left self) left)
        (.right self) (* (.right self) right))
  (when (= (incf (.in-count self))
           (length (.parents self)))
    (play-children self (.left self) (.right self))
    (setf (.in-count self) 0)
    (setf (.left self) 1.0d0
          (.right self) 1.0d0)))

(defclass out (module)
  ((left :initform 0.0d0 :accessor .left)
   (right :initform 0.0d0 :accessor .right)
   (volume :initform 0.6d0 :accessor .volume)))

(defmethod play ((self out) left right)
  (incf (.left self) left)
  (incf (.right self) right))

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

(defmacro with-audio (&body body)
  `(portaudio:with-audio
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
                       :frames-per-buffer (.frames-per-buffer *audio*))))
              (setf (.playing *audio*) t)
              (setf (.start-time *audio*) 0.0d0)
              ,@body))
       (when (.stream *audio*)
         (pa:close-stream (.stream *audio*))))))

(defun scratch-audio ()
  ;;(declare (optimize (speed 3) (safety 0)))
  (let* ((*audio* (setf *audio* (make-instance 'audio)))
         (out (.out *audio*))
         (pattern1 (make-instance 'pattern
                                  :lines (list a4 e4 none g4
                                               a4 off  g4 c4)))
         (osc1 (make-instance 'sin-wave))
         (adsr1 (make-instance 'adsr :d 0.2d0 :s 0d0))
         (amp1 (make-instance 'amp))
         (pattern2 (make-instance 'pattern
                                  :lines (list a3 e3 none g3
                                               a3 off  g3 c3)))
         (osc2 (make-instance 'saw-wave))
         (adsr2 (make-instance 'adsr :d 0.7d0 :s 0.8d0))
         (amp2 (make-instance 'amp)))
    (connect pattern1 osc1)
    (connect pattern1 adsr1)
    (connect osc1 amp1)
    (connect adsr1 amp1)
    (connect amp1 out)
    (connect pattern2 osc2)
    (connect pattern2 adsr2)
    (connect osc2 amp2)
    (connect adsr2 amp2)
    (connect amp2 out)
    (add-pattern (.sequencer *audio*) pattern1 0)
    (add-pattern (.sequencer *audio*) pattern1 (length (.lines pattern1)))
    (add-pattern (.sequencer *audio*) pattern2 (length (.lines pattern1)))
    (add-pattern (.sequencer *audio*) pattern1 (* 2 (length (.lines pattern1))))
    (add-pattern (.sequencer *audio*) pattern2 (* 2 (length (.lines pattern1))))
    (with-audio
      (unwind-protect
           (progn
             (pa:start-stream (.stream *audio*))
             (loop while (.playing *audio*) do (pa:pa-sleep 10)))
        (when (.stream *audio*)
          (pa::stop-stream (.stream *audio*)))))))
