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
   (bpm :initarg :bpm :initform 120.0d0 :accessor .bpm
        :type double-float)
   (lpb :initarg :lpb :initform 4 :accessor .lpb)
   (patterns :initarg :patterns
             :initform (list (make-instance 'pattern))
             :accessor .patterns)))

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

(defclass pattern ()
  ((length :initarg :length :initform 10 :accessor .length)
   (contents :initarg :contents
             :initform (list a4 a4 e4 g4 a4 e4 e4 g4 c4 c4) :accessor .contents)
   (children :initform (list (make-instance 'sin-wave)) :accessor .children)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod play ((pattern pattern) line frame)
  (let* ((note (nth (mod line (.length pattern)) (.contents pattern))))
    (if note
        (loop for i below (* (.frames-per-buffer *audio*) 2) by 2
              with freq = (midino-to-freq note)
              do (%play (car (.children pattern)) freq))
        (loop for i below (* (.frames-per-buffer *audio*) 2)
              do (push-to-buffer 0.0d0)))))

(defclass sin-wave ()
  ((freq :initarg :freq :initform 440.0d0 :accessor .freq
         :type double-float)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)
   (children :initform (list (make-instance 'adsr)) :accessor .children)))

(defmethod %play ((sin-wave sin-wave) freq)
  (when (/= (.freq sin-wave) freq)
    (setf (.freq sin-wave) freq
          (.phase sin-wave) 0
          (.frame (car (.children sin-wave))) 0))
  (let ((value (sin (* (/ (* 2 pi freq) (.sample-rate *audio*))
                       (.phase sin-wave)))))
    (loop for child in (.children sin-wave)
          do (%play child value))
    (incf (.phase sin-wave))))

(defclass adsr ()
  ((a :initarg :a :initform 0.001d0 :accessor .a)
   (d :initarg :d :initform 0.05d0 :accessor .d)
   (s :initarg :s :initform 0.3d0 :accessor .s)
   (r :initarg :r :initform 0.1d0 :accessor .r)
   (frame :initform 0 :accessor .frame)
   (children :initform (list (make-instance 'out)) :accessor .children)))

(defmethod %play ((self adsr) value)
  (let* ((sec-per-frame (/ 1.0d0 (.sample-rate *audio*)))
         (current (* sec-per-frame (.frame self)))
         (x (cond ((< current (.a self))
                   (* (/ 1.0d0 (.a self)) current))
                  ((< current (+ (.a self) (.d self)))
                   (- 1.0d0 (* (/ (- 1.0d0 (.s self)) (.d self))
                               (- current (.a self)))))
                  ((< current (+ (.a self) (.d self) (.r self)))
                   (- (.s self)
                         (* (/ (.s self) (.r self))
                            (- current (.a self) (.d self)))))
                  (t 0d0)))
         (value (* value x)))
    (loop for child in (.children self)
          do (%play child value))
    (incf (.frame self))))

(defclass out ()
  ((volume :initform 0.6d0 :accessor .volume)))

(defmethod %play ((out out) value)
  ;;left
  (push-to-buffer (* value (.volume out)))
  ;;right
  (push-to-buffer (* value (.volume out))))

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
   (play (car (.patterns *audio*)) line frame))
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

(defun callback-test ()
  ;;(declare (optimize (speed 3) (safety 0)))
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
          (setf (.start-time *audio*) 0.0d0)
          (pa:start-stream (.stream *audio*))
          ;; (loop while *playing* do (pa:pa-sleep 100))
          (pa:pa-sleep 3000))
        (when (.stream *audio*)
          (pa::stop-stream (.stream *audio*))
          (pa:close-stream (.stream *audio*)))))))
