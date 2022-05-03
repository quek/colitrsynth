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
   (bpm :initarg :bpm :initform 128.0d0 :accessor .bpm
        :type double-float)
   (patterns :initarg :patterns
             :initform (list (make-instance 'pattern))
             :accessor .patterns)))

(defclass pattern ()
  ((contents :initarg :contents
             :initform (list a4 e4 g4) :accessor .contents)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod play ((pattern pattern))
  (let* ((current-time (.current-time *audio*))
         (note (nth (cond ((< current-time 1.0d0) 0)
                          ((< current-time 2.0d0) 1)
                          (t 2))
                    (.contents pattern))))
    (loop for i below (* (.frames-per-buffer *audio*) 2) by 2
          for phase from (.phase pattern) by (/ (* 2 pi (midino-to-freq note))
                                                (.sample-rate *audio*))
          for freq = (coerce (* (sin phase) 0.5) 'single-float)
          do (setf (cffi:mem-aref (.buffer *audio*) :float i)
                   freq)
             (setf (cffi:mem-aref (.buffer *audio*) :float (1+ i))
                   freq)
          finally (setf (.phase pattern) phase))))

(defun proc (input-buffer
             output-buffer
             time-info
             status-flags)
  (declare (optimize (speed 3) (safety 0))
           (ignore input-buffer status-flags))
  (let ((current-time (cffi:foreign-slot-value time-info '(:struct pa-stream-callback-time-info)
                                               'current-time)))
    (declare (double-float current-time))
    (when (= (the double-float (.start-time *audio*)) 0.0d0)
      (setf (.start-time *audio*) current-time))
    (setf (.current-time *audio*) (the double-float (- current-time (the double-float (.start-time *audio*)))))
    (setf (.buffer *audio*) output-buffer)
    ;; (cffi:with-foreign-slots ((input-buffer-adc-time current-time output-buffer-dac-time)
    ;;                           time-info
    ;;                           (:struct pa-stream-callback-time-info))
    ;;   (format t "~&~a ~a ~a" input-buffer-adc-time current-time output-buffer-dac-time))
    #+nil
    (let* (
           (delta (- current-time (the double-float (.start-time *audio*))))
           (vol 0.4)
           (env (if (> delta 1.0d0)
                    1.0
                    2.0)))
      (when (> delta 2)
        (setf (.start-time *audio*) current-time))
      fixnum))
  (play (car (.patterns *audio*)))
  0)

(cffi:defcallback my-callback :int ((input-buffer :pointer)
                                    (output-buffer :pointer)
                                    (frame-per-buffer :unsigned-long)
                                    (time-info (:pointer (:struct pa-stream-callback-time-info)))
                                    (status-flags pa-stream-callback-flags)
                                    (user-data :pointer))
  (declare (ignore frame-per-buffer ;*audio* の方を参照するので無視でいい？
                   user-data))
  (funcall 'proc
           input-buffer
           output-buffer
           time-info
           status-flags))

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
          (setf (.start-time *audio*) 0.0d0)
          (pa:start-stream (.stream *audio*))
          ;; (loop while *playing* do (pa:pa-sleep 100))
          (pa:pa-sleep 3000))
        (when (.stream *audio*)
          (pa::stop-stream (.stream *audio*))
          (pa:close-stream (.stream *audio*)))))))

(defun midino-to-freq (midino)
  (declare (optimize (speed 3) (safety 0))
           (fixnum midino))
  (the double-float
       (* 440.0d0
          (expt 2.0d0
                (the double-float (/ (coerce (the fixnum (- midino 69)) 'double-float)
                                     12.0d0))))))

(defconstant c0  12)
(defconstant c#0 13)
(defconstant d0  14)
(defconstant d#0 15)
(defconstant e0  16)
(defconstant f0  17)
(defconstant f#0 18)
(defconstant g0  19)
(defconstant g#0 20)
(defconstant a0  21)
(defconstant a#0 22)
(defconstant b0  23)

(defconstant c1  24)
(defconstant c#1 25)
(defconstant d1  26)
(defconstant d#1 27)
(defconstant e1  28)
(defconstant f1  29)
(defconstant f#1 30)
(defconstant g1  31)
(defconstant g#1 32)
(defconstant a1  33)
(defconstant a#1 34)
(defconstant b1  35)

(defconstant c2  36)
(defconstant c#2 37)
(defconstant d2  38)
(defconstant d#2 39)
(defconstant e2  40)
(defconstant f2  41)
(defconstant f#2 42)
(defconstant g2  43)
(defconstant g#2 44)
(defconstant a2  45)
(defconstant a#2 46)
(defconstant b2  47)

(defconstant c3  48)
(defconstant c#3 49)
(defconstant d3  50)
(defconstant d#3 51)
(defconstant e3  52)
(defconstant f3  53)
(defconstant f#3 54)
(defconstant g3  55)
(defconstant g#3 56)
(defconstant a3  57)
(defconstant a#3 58)
(defconstant b3  59)

(defconstant c4  60)
(defconstant c#4 61)
(defconstant d4  62)
(defconstant d#4 63)
(defconstant e4  64)
(defconstant f4  65)
(defconstant f#4 66)
(defconstant g4  67)
(defconstant g#4 68)
(defconstant a4  69)
(defconstant a#4 70)
(defconstant b4  71)

(defconstant c5  72)
(defconstant c#5 73)
(defconstant d5  74)
(defconstant d#5 75)
(defconstant e5  76)
(defconstant f5  77)
(defconstant f#5 78)
(defconstant g5  79)
(defconstant g#5 80)
(defconstant a5  81)
(defconstant a#5 82)
(defconstant b5  83)

(defconstant c6  84)
(defconstant c#6 85)
(defconstant d6  86)
(defconstant d#6 87)
(defconstant e6  88)
(defconstant f6  89)
(defconstant f#6 90)
(defconstant g6  91)
(defconstant g#6 92)
(defconstant a6  93)
(defconstant a#6 94)
(defconstant b6  95)

(defconstant c7  96)
(defconstant c#7 97)
(defconstant d7  98)
(defconstant d#7 99)
(defconstant e7  100)
(defconstant f7  101)
(defconstant f#7 102)
(defconstant g7  103)
(defconstant g#7 104)
(defconstant a7  105)
(defconstant a#7 106)
(defconstant b7  107)

(defconstant c8  108)
(defconstant c#8 109)
(defconstant d8  110)
(defconstant d#8 111)
(defconstant e8  112)
(defconstant f8  113)
(defconstant f#8 114)
(defconstant g8  115)
(defconstant g#8 116)
(defconstant a8  117)
(defconstant a#8 118)
(defconstant b8  119)

(defconstant c9  120)
(defconstant c#9 121)
(defconstant d9  122)
(defconstant d#9 123)
(defconstant e9  124)
(defconstant f9  125)
(defconstant f#9 126)
(defconstant g9  127)
(defconstant g#9 128)
(defconstant a9  129)
(defconstant a#9 130)
(defconstant b9  131)





