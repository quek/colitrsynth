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

(defparameter *frames-per-buffer* 1024)
(defparameter *sample-rate* 48000.0d0)
(defun sec-per-line ()
  (/ 60.0d0 (.bpm *audio*) (.lpb *audio*)))
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
  (let* ((sec-per-line (sec-per-line))
         (sec-per-frame (sec-per-frame))
         (current-sec (* sec-per-frame (.nframes *audio*))))
    (let ((line (floor (/ current-sec sec-per-line)))
          (frame (floor (/ (mod current-sec sec-per-line) sec-per-frame)))) 
      (values line frame))))

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
  ((pattern-positions :initform nil :accessor .pattern-positions)
   (buffer :initform (make-buffer :initial-element nil :element-type t) :accessor .buffer)))

(defmethod play-frame ((self track) line frame)
  (let ((midi-events
          (loop for pattern-position in (.pattern-positions self)
                nconc (with-slots (pattern start end) pattern-position
                        (if (and (<= start line)
                                 (< line end))
                            (midi-events-at-line-frame pattern (- line start) frame)
                            nil)))))
    (route self midi-events frame)))

(defclass sequencer (audio-module)
  ((tracks :initarg :tracks :accessor .tracks :initform nil)
   (end :initform 0 :accessor .end)
   (loop :initform t :accessor .loop)
   (current-line :initform 0 :accessor .current-line)))

(defun update-sequencer-end ()
  (let ((sequencer (.sequencer *audio*)))
    (setf (.end sequencer)
          (loop for track in (.tracks sequencer)
                maximize (loop for pattern-position in (.pattern-positions track)
                               maximize (.end pattern-position))))))

(defun play-sequencer (self line frame)
  (let ((end (.end self)))
    (if (zerop end)
        (progn
          (write-master-buffer)
          (setf (.current-line self) 0))
        (let* ((line (if (.loop self)
                         (mod line end)
                         line)))
          (if (< end line)
              (progn
                ;; TODO reverb とか残す？
                (write-master-buffer)
                (request-stop))
              (progn
                (loop for track in (.tracks self)
                      do (play-frame track line frame))
                (write-master-buffer)
                (setf (.current-line self) line)))))))

(defclass line ()
  ((note :initarg :note :initform none :accessor .note)))

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

(defmethod add-pattern ((track track) (pattern pattern) start end)
  (let ((pattern-position (make-instance 'pattern-position
                                         :pattern pattern
                                         :start start :end end)))
    (push pattern-position
          (.pattern-positions track))
    (update-sequencer-end)
    pattern-position))

(defmethod remove-pattern ((track track) (pattern-position pattern-position-mixin))
  (setf (.pattern-positions track)
        (remove pattern-position (.pattern-positions track)))
  (update-sequencer-end))

(defun midi-events-at-line-frame (pattern line frame)
  (setf (.current-line pattern) line)
  (let* ((frames-per-line (frames-per-line))
         (start-line (if (zerop frame)
                         line
                         (1+ line)))
         (end-line (+ start-line (floor (/ (- *frames-per-buffer* frame) frames-per-line))))
         events)
    (loop for current-line from start-line to (min end-line (1- (.length pattern)))
          for current-frame = (- (* (- current-line line) frames-per-line)
                                 frame)
          for note = (.note (aref (.lines pattern) current-line))
          for last-note = (.last-note pattern)
          if (<= c0 note)
            do (push (make-instance 'midi-event :event +midi-event-on+
                                                :note note
                                                :frame current-frame)
                     events)
          if (and (= note off) (<= c0 last-note))
            do (push (make-instance 'midi-event :event +midi-event-off+
                                                :note last-note
                                                :frame current-frame)
                     events)
          do (setf (.last-note pattern) note))
    (nreverse events)))

(defclass osc (audio-module)
  ((note :initarg :note :initform c4 :accessor .note)
   (buffer :initform (make-buffer) :accessor .buffer)
   (value :initform 0.0d0 :accessor .value)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod play-frame ((self osc) midi-events frame)
  (loop for i below *frames-per-buffer*
        for note = (loop for x in midi-events
                           thereis (and (= (.event x) +midi-event-on+)
                                        (= (.frame x) (+ frame i))
                                        (.note x)))
        do (when note
             (setf (.phase self) 0)
             (setf (.note self) note))
           (let ((value (osc-frame-value self)))
             (setf (aref (.buffer self) i) value)
             (setf (.value self) value)
             (incf (.phase self))))
  (route self (.buffer self) (.buffer self)))

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
   (buffer :initform (make-buffer) :accessor .buffer)
   (last-gate :initform nil :accessor .last-gate)
   (frame :initform 0 :accessor .frame)
   (release-time :initform 0.0d0 :accessor .release-time)))

(defmethod play-frame ((self adsr) midi-events frame)
  (loop for i below *frames-per-buffer*
        for midi-event = (loop for x in midi-events
                                 thereis (and (or (= (.event x) +midi-event-on+)
                                                  (= (.event x) +midi-event-off+))
                                              (= (.frame x) (+ frame i))
                                              x))
        for gate = (if midi-event
                       (= (.event midi-event) +midi-event-on+)
                       (.last-gate self))
        with sec-per-frame = (/ 1.0d0 *sample-rate*)
        for current = (* sec-per-frame (.frame self))
        for value = (if gate
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
                                0.0d0))))
        do (setf (aref (.buffer self) i) value)
           (incf (.frame self))
           (setf (.last-gate self) gate)))

(defclass amp (audio-module)
  ((left :initform (make-buffer) :accessor .left)
   (right :initform (make-buffer) :accessor .right)
   (in-count :initform 0 :accessor .in-count)))

(defmethod play-frame ((self amp) left right)
  (loop for i below *frames-per-buffer*
        do (setf (aref (.left self) i)
                 (* (aref (.left self) i)
                    (aref left i))
                 (aref (.right self) i)
                 (* (aref (.right self) i)
                    (aref right i))))
  (when (<= (length (.in self))
            (incf (.in-count self)))
    (route self (.left self) (.right self))
    (setf (.in-count self) 0)
    (loop for i below *frames-per-buffer*
          do (setf (aref (.left self) i) 1.0d0
                   (aref (.right self) i) 1.0d0))))

(defclass master (audio-module)
  ((left :initform (make-buffer) :accessor .left)
   (right :initform (make-buffer) :accessor .right)
   (volume :initform 0.6d0 :accessor .volume)))

(defmethod play-frame ((self master) left right)
  (loop for i below *frames-per-buffer*
        do (incf (aref (.left self) i) (aref left i))
           (incf (aref (.right self) i) (aref right i))))

(defun proc (input-buffer
             output-buffer
             frame-per-buffer
             time-info
             status-flags)
  (declare ;; (optimize (speed 3) (safety 0))
   (ignore input-buffer status-flags))
  (assert (= frame-per-buffer *frames-per-buffer*))
  (let ((current-time (cffi:foreign-slot-value time-info '(:struct pa-stream-callback-time-info)
                                               'current-time)))
    (declare (double-float current-time))
    (when (= (the double-float (.start-time *audio*)) 0.0d0)
      (setf (.start-time *audio*) current-time))
    (setf (.current-time *audio*) (the double-float (- current-time (the double-float (.start-time *audio*)))))
    (setf (.buffer *audio*) output-buffer))

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
      (add-pattern track1 pattern1 0 line-length)
      (add-pattern track1 pattern1 line-length (* 2 line-length))
      (add-pattern track2 pattern2 line-length (* 2 line-length))
      (add-pattern track1 pattern1 (* 2 line-length) (* 3 line-length))
      (add-pattern track2 pattern2 (* 2 line-length) (* 3 line-length))
      (setf (.loop sequencer) nil)
      (play)
      (loop until (.request-stop *audio*) do (pa:pa-sleep 10)))))
;;(setf (.request-stop *audio*) t)
