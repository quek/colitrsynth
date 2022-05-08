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
   (buffer-index :initform 0 :accessor .buffer-index :type fixnum)
   (bpm :initarg :bpm :initform 90.0d0 :accessor .bpm
        :type double-float)
   (lpb :initarg :lpb :initform 4 :accessor .lpb)
   (sequencer :initarg :sequencer :accessor .sequencer
              :initform (make-instance 'sequencer-module :x 5 :y 5))
   (master :accessor .master
           :initform (make-instance 'master-module :x 695 :y 515))))

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
  (let* ((sec-per-line (/ 60.0d0 (.bpm *audio*) (.lpb *audio*)))
         (sec-per-frame (/ 1.0d0 (.sample-rate *audio*)))
         (current-sec (* sec-per-frame (.nframes *audio*))))
    (multiple-value-bind (line remain) (floor (/ current-sec sec-per-line))
      (values line (floor (/ remain sec-per-frame))))))

(defun push-to-buffer (value)
  (setf (cffi:mem-aref (.buffer *audio*) :float (.buffer-index *audio*))
        (coerce value 'single-float))
  (incf (.buffer-index *audio*)))

(defun write-master-buffer ()
  (let* ((master (.master *audio*))
         (volume (.volume master))
         (left (* (.left master) volume))
         (right (* (.right master) volume)))
    (when (< 1.0d0 left)
      (warn "音大きすぎ ~a" left)
      (setf left 1d0))
    (when (< left -1.0d0)
      (warn "音大きすぎ ~a" left)
      (setf left -1d0))
    (when (< 1.0d0 right)
      (warn "音大きすぎ ~a" right)
      (setf right 1d0))
    (when (< right -1.0d0)
      (warn "音大きすぎ ~a" right)
      (setf right -1d0))
    (push-to-buffer left)
    (push-to-buffer right)
    (setf (.left master) 0.0d0
          (.right master) 0.0d0)))

(defclass audio-module ()
  ((in :initarg :in :accessor .in :initform nil)
   (out :initarg :out :accessor .out :initform nil)))

(defmethod connect ((in audio-module) (out audio-module))
  (push out (.out in))
  (push in (.in out)))

(defmethod disconnect ((in audio-module) (out audio-module))
  (setf (.out in) (remove out (.out in)))
  (setf (.in out) (remove in (.in out))))

(defmethod route ((self audio-module) left right)
  (loop for out in (.out self)
        do (play-frame out left right)))

(defclass pattern-position-mixin ()
  ((pattern :initarg :pattern :accessor .pattern)
   (start :initarg :start :accessor .start)
   (end :initarg :end :accessor .end)))

(defgeneric play-frame (audio-module left right))

(defclass track (audio-module)
  ((pattern-positions :initform nil :accessor .pattern-positions)))

(defmethod play-frame ((self track) line frame)
  (loop for pattern-position in (.pattern-positions self)
        do (with-slots (pattern start end) pattern-position
             (when (and (<= start line)
                        (< line end))
               (multiple-value-call #'route self
                 (note-gate-at-line-frame pattern (- line start) frame))))))

(defclass sequencer (audio-module)
  ((tracks :initarg :tracks :accessor .tracks :initform nil)
   (end :initform 0 :accessor .end)
   (loop :initform t :accessor .loop)))

(defun play-sequencer (self line frame)
  (let* ((end (.end self))
         (line (if (.loop self)
                   (mod line end)
                   line)))
    (if (< end line)
        (progn
          ;; TODO reverb とか残す？
          (loop for i below (.frames-per-buffer *audio*)
                do (write-master-buffer))
          (request-stop))
        (loop for i below (.frames-per-buffer *audio*)
              do (loop for track in (.tracks self)
                       do (play-frame track line frame))
                 (write-master-buffer)))))

(defclass line ()
  ((note :initarg :note :initform off :accessor .note)))

(defclass pattern (audio-module)
  ((length :initarg :length :initform #x40 :accessor .length)
   (lines :initarg :lines :accessor .lines)
   (current-line :initform 0 :accessor .current-line)
   (last-note :initform off :accessor .last-note)))

(defmethod initialize-instance :after ((self pattern) &key)
  (unless (slot-boundp self 'lines)
    (setf (.lines self)
          (make-array (.length self)
                      :initial-contents 
                      (loop repeat (.length self)
                            collect (make-instance 'line))))))

(defmethod add-pattern ((sequencer sequencer) (track track) (pattern pattern) start end)
  (let ((pattern-position (make-instance 'pattern-position
                                         :pattern pattern
                                         :start start :end end)))
   (push pattern-position
         (.pattern-positions track))
    (setf (.end sequencer) (max (.end sequencer) end))
    pattern-position))

(defun note-gate-at-line-frame (pattern line frame)
  (declare (ignore frame))              ;TODO
  (setf (.current-line pattern) line)
  (let* ((note (.note (aref (.lines pattern) line)))
         (note (if (= note none)
                   (.last-note pattern)
                   note))
         (gate (and (/= note off)
                    ;;こんな実装でいいの？
                    (= (.last-note pattern) note))))
    (setf (.last-note pattern) note)
    (values note gate)))

(defclass osc (audio-module)
  ((note :initarg :note :initform off :accessor .note)
   (value :initform 0.0d0 :accessor .value)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod play-frame ((self osc) note gate)
  (declare (ignore gate))
  (let ((value
          (if (= note off)
              0.0d0
              (progn
                (when (/= (.note self) note)
                  (setf (.phase self) 0))
                (setf (.note self) note)
                (osc-frame-value self)))))
    (route self value value)
    (setf (.value self) value))
  (incf (.phase self)))

(defclass sin-osc (osc)
  ())

(defmethod osc-frame-value ((self sin-osc))
  (sin (* (/ (* 2 pi (midino-to-freq (.note self))) (.sample-rate *audio*))
          (.phase self))))

(defclass saw-osc (osc)
  ())

(defmethod osc-frame-value ((self saw-osc))
  (* 0.3d0        ;TODO 音大きいのでとりあえずつけとく。本来はいらない？
     (- (* (mod (/ (* (.phase self) (midino-to-freq (.note self)))
                   (.sample-rate *audio*))
                1d0)
           2d0)
        1d0)))

(defclass adsr (audio-module)
  ((a :initarg :a :initform 0.003d0 :accessor .a)
   (d :initarg :d :initform 0.05d0 :accessor .d)
   (s :initarg :s :initform 0.3d0 :accessor .s)
   (r :initarg :r :initform 0.1d0 :accessor .r)
   (last-gate :initform nil :accessor .last-gate)
   (frame :initform 0 :accessor .frame)
   (release-time :initform nil :accessor .release-time)
   (release-value :initform nil :accessor .release-value)))

(defmethod play-frame ((self adsr) note gate)
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
    (route self value value)
    (incf (.frame self))
    (setf (.last-gate self) gate)))

(defclass amp (audio-module)
  ((left :initform 1.0d0 :accessor .left)
   (right :initform 1.0d0 :accessor .right)
   (in-count :initform 0 :accessor .in-count)))

(defmethod play-frame ((self amp) left right)
  (setf (.left self) (* (.left self) left)
        (.right self) (* (.right self) right))
  (when (<= (length (.in self))
            (incf (.in-count self)))
    (route self (.left self) (.right self))
    (setf (.in-count self) 0)
    (setf (.left self) 1.0d0
          (.right self) 1.0d0)))

(defclass master (audio-module)
  ((left :initform 0.0d0 :accessor .left)
   (right :initform 0.0d0 :accessor .right)
   (volume :initform 0.6d0 :accessor .volume)))

(defmethod play-frame ((self master) left right)
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
                ,@body)
           (when (.stream *audio*)
             (stop)
             (pa:close-stream (.stream *audio*))))))))

(defun list-to-pattern-lines (list)
  (make-array (length list)
              :initial-contents 
              (loop for x in list
                    collect (make-instance 'line :note x))))

(defun scratch-audio ()
  ;;(declare (optimize (speed 3) (safety 0)))
  (with-audio
    (let* ((line-length 8)
           (sequencer (.sequencer *audio*))
           (track1 (car (push (make-instance 'track) (.tracks sequencer))))
           (track2 (car (push (make-instance 'track) (.tracks sequencer))))
           (master (.master *audio*))
           (pattern1 (make-instance
                      'pattern
                      :length line-length
                      :lines (list-to-pattern-lines
                              (list a4 e4 none g4
                                    a4 off  g4 c4))))
           (osc1 (make-instance 'sin-osc))
           (adsr1 (make-instance 'adsr :d 0.2d0 :s 0d0))
           (amp1 (make-instance 'amp))
           (pattern2 (make-instance
                      'pattern
                      :length line-length
                      :lines (list-to-pattern-lines
                              (list a3 e3 none g3
                                    a3 off  g3 c3))))
           (osc2 (make-instance 'saw-osc))
           (adsr2 (make-instance 'adsr :d 0.7d0 :s 0.8d0))
           (amp2 (make-instance 'amp)))
      (connect track1 osc1)
      (connect track1 adsr1)
      (connect osc1 amp1)
      (connect adsr1 amp1)
      (connect amp1 master)
      (connect track2 osc2)
      (connect track2 adsr2)
      (connect osc2 amp2)
      (connect adsr2 amp2)
      (connect amp2 master)
      (add-pattern sequencer track1 pattern1 0 line-length)
      (add-pattern sequencer track1 pattern1 line-length (* 2 line-length))
      (add-pattern sequencer track2 pattern2 line-length (* 2 line-length))
      (add-pattern sequencer track1 pattern1 (* 2 line-length) (* 3 line-length))
      (add-pattern sequencer track2 pattern2 (* 2 line-length) (* 3 line-length))
      (setf (.loop sequencer) nil)
      (play)
      (loop until (.request-stop *audio*) do (pa:pa-sleep 10)))))
;;(setf (.request-stop *audio*) t)
