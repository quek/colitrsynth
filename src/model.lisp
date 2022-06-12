(in-package :colitrsynth)

(defparameter *plugin-host-exe*
  "C:/Users/ancient/Documents/Visual Studio 2022/PluginHost/Builds/VisualStudio2022/x64/Debug/App/PluginHost.exe"
  ;;"C:/Users/ancient/Documents/Visual Studio 2022/PluginHost/Builds/VisualStudio2022/x64/Release/App/PluginHost.exe"
  )
(defparameter *plugin-host-pipe-name* "\\\\.\\pipe\\pluin-host")


(defgeneric initialize (x))
(defgeneric connect (connection))
(defgeneric disconnect (connection))
(defgeneric disconnect-all (model))
(defgeneric process (model connection left right)
  (:method (model (connection null) left route)))
(defgeneric route (self left right))
(defgeneric prepare-save (model))


(defmethod connection-eql (a b)
  (and (eql (type-of a) (type-of b))
       (eql (.src a) (.src b))
       (eql (.dest a) (.dest b))))

(defmethod connection-eql ((a audio-connection) (b audio-connection))
  (and (call-next-method)
       (= (.src-bus a)
          (.src-bus b))
       (= (.dest-bus a)
          (.dest-bus b))))

(defmethod connection-eql ((a builtin-param-connection) (b builtin-param-connection))
  (eql (builtin-parameter-accessor (.param a))
       (builtin-parameter-accessor (.param b))))

(defmethod connection-eql ((a plugin-param-connection) (b plugin-param-connection))
  (= (plugin-parameter-index (.param a))
     (plugin-parameter-index (.param b))))

(defmethod default-connection-class (src dest)
  'audio-connection)


(defmethod connect (connection)
  (push connection (.out (.src connection)))
  (push connection (.in (.dest connection))))

(defmethod connected-p (connection)
  (member connection (.in (.dest connection))
          :test #'connection-eql))

(defmethod disconnect (connection)
  (setf (.out (.src connection))
        (remove connection (.out (.src connection))
                :test #'connection-eql))
  (setf (.in (.dest connection))
        (remove connection (.in (.dest connection))
                :test #'connection-eql)))

(defmethod disconnect-all ((self model))
  (loop for connection in (copy-list (.in self))
        do (disconnect connection))
  (loop for connection in (copy-list (.out self))
        do (disconnect connection)))

(defmethod route ((self model) left right)
  (loop for connection in (.out self)
        do (route-connection connection
                             (.src connection)
                             (.dest connection)
                             left
                             right)))

(defmethod route-connection (connection src dest left right)
  (process dest connection left right))

(defmethod route-connection ((connection audio-connection)
                             (src plugin-model)
                             dest
                             left right)
  (let ((bus (.src-bus connection)))
    (process dest
             connection
             (aref left bus)
             (aref right bus))))

(defmethod close ((self model) &key abort)
  (declare (ignore abort)))

(defmethod prepare-save ((self model)))



(defmethod .name ((self pattern-position))
  (.name (.pattern self)))


(defmethod default-connection-class ((src track) dest)
  'midi-connection)

(defun play-track (track start-line start-frame end-line end-frame)
  (let* ((frames-per-line (frames-per-line))
         (midi-events
           (loop for pattern-position in (.pattern-positions track)
                 nconc (with-slots (start end) pattern-position
                         (cond ((< end-line start-line) ;ループしている場合
                                (append
                                 (if (and (<= start start-line)
                                          (< start-line end))
                                     (midi-events-at-line-frame pattern-position
                                                                (- start-line start) start-frame
                                                                (- start-line start) frames-per-line))
                                 (if (and (<= start end-line)
                                          (< end-line end))
                                     (midi-events-at-line-frame pattern-position
                                                                (- end-line start) 0
                                                                (- end-line start)
                                                                end-frame))))
                               ((and (<= start end-line)
                                     (< start-line end))
                                (let ((start-line (- start-line start))
                                      (start-frame start-frame))
                                  (when (minusp start-line)
                                    (setf start-line 0)
                                    (setf start-frame 0))
                                  (midi-events-at-line-frame pattern-position
                                                             start-line start-frame
                                                             (- end-line start) end-frame))))))))
    #+nil
    (when midi-events
      (print midi-events))
    (route track midi-events start-frame)))

(defun play-track-all-off (track start-frame)
  (route track (list (midi-event-all-notes-off)) start-frame))

(defun play-track-no-notes (track start-frame)
  (route track nil start-frame))

(defstruct play-position
  (line 0)
  (line-frame 0))

(declaim (ftype (function (play-position) fixnum) as-frame))
(defun as-frame (play-position)
  (let* ((start-line (play-position-line play-position))
         (start-line-frame (play-position-line-frame play-position))
         (sec-per-line (sec-per-line))
         (sec-per-frame (sec-per-frame))
         (start-sec (+ (* sec-per-line start-line)
                       (* sec-per-frame start-line-frame)))
         (start-frame (/ start-sec sec-per-frame)))
    (floor start-frame)))

(defun inc-frame (play-position)
  (let* ((start-line (play-position-line play-position))
         (start-line-frame (play-position-line-frame play-position))
         (sec-per-line (sec-per-line))
         (sec-per-frame (sec-per-frame))
         (start-sec (+ (* sec-per-line start-line)
                       (* sec-per-frame start-line-frame)))
         (start-frame (/ start-sec sec-per-frame))
         (end-frame (+ start-frame *frames-per-buffer*))
         (end-sec (* end-frame sec-per-frame))
         (end-line (floor (/ end-sec sec-per-line)))
         (end-line-frame (floor (/ (- end-sec (* sec-per-line end-line))
                                   sec-per-frame))))
    (make-play-position :line end-line :line-frame end-line-frame)))


(defun current-frame (sequencer)
  (as-frame (.play-position sequencer)))

(defun update-sequencer-end (sequencer)
  (setf (.end sequencer)
        (loop for track in (.tracks sequencer)
              maximize (loop for pattern-position in (.pattern-positions track)
                             maximize (.end pattern-position)))))

(defun process-sequencer (self start-line start-frame end-line end-frame)
  (let ((end (.end self)))
    (if (or (zerop end)
            (< end start-line))
        (loop for track in (.tracks self)
              do (play-track-no-notes track start-frame))
        (progn
          (cond ((playing)
                 (loop for track in (.tracks self)
                       do (play-track track start-line start-frame end-line end-frame)))
                ((played)
                 (loop for track in (.tracks self)
                       do (play-track-all-off track start-frame)))
                (t
                 (loop for track in (.tracks self)
                       do (play-track-no-notes track start-frame)))))))
  ;; LFO など入力なしでも動くモジュール
  (loop for model in (.modules *app*)
        if (null (.in model))
          do (process model nil nil nil))
  (write-master-buffer))


(defmethod initialize-instance :after ((self pattern) &key)
  (unless (slot-boundp self 'lines)
    (setf (.lines self)
          (make-array (.length self)
                      :initial-contents 
                      (loop repeat (.length self)
                            collect (make-instance 'line))))))

(defmethod extend-column ((self pattern))
  (loop with lines = (.lines self)
        with line-0 = (aref lines 0)
        with length = (min (1+ (.length line-0))
                           (length (.columns line-0)))
        for line across lines
        do (setf (.length line) length)))

(defun midi-events-at-line-frame (pattern-position start-line start-frame end-line end-frame)
  (declare (ignore end-frame))
  (setf (.current-line (.pattern pattern-position)) start-line)
  (let* ((pattern (.pattern pattern-position))
         (frames-per-line (frames-per-line))
         (arg-start-line start-line)
         (start-line (if (zerop start-frame)
                         start-line
                         (1+ start-line)))
         events)
    (loop for current-line from start-line to (min end-line (1- (.length pattern)))
          for current-frame = (floor (- (* (- current-line arg-start-line) frames-per-line)
                                        start-frame))
          for line = (aref (.lines pattern) current-line)
          do (loop for column across (.columns line)
                   for i below (.length line)
                   for note = (.note column)
                   for last-note = (aref (.last-notes pattern-position) i)
                   if (or (and (<= c0 note)
                               (<= c0 last-note)
                               (/= note last-note))
                          (and (= note off) (<= c0 last-note)))
                     do (push (make-instance 'midi-event :event +midi-event-off+
                                                         :note last-note
                                                         :velocity 0
                                                         :frame current-frame)
                              events)
                   if (<= c0 note)
                     do (push (make-instance 'midi-event :event +midi-event-on+
                                                         :note note
                                                         :velocity (.velocity column)
                                                         :frame current-frame)
                              events)
                   if (/= note none)
                     do (setf (aref (.last-notes pattern-position) i) note)))
    (sort events (lambda (a b) (if (= (.frame a) (.frame b))
                                   (< (.event a) (.event b))
                                   (< (.frame a) (.frame b)))))))

(defmethod process ((self osc) (connection midi-connection) midi-events frame)
  (flet ((midi-event (i on-or-off)
           (loop for x in midi-events
                   thereis (and (= (.event x) on-or-off)
                                (= (.frame x) (mod (+ frame i) *frames-per-buffer*))
                                x))))
    (loop for i below *frames-per-buffer*
          for on-event = (midi-event i +midi-event-on+)
          for off-event = (midi-event i +midi-event-off+)
          for value = (cond (on-event
                             (setf (.phase self) 0
                                   (.note self) (.note on-event))
                             (osc-frame-value self))
                            (off-event
                             (setf (.note self) off)
                             0.0d0)
                            (t
                             (if (= off (.note self))
                                 0.0d0
                                 (osc-frame-value self))))
          do (setf (aref (.buffer self) i) value)
             (setf (.value self) value)
             (incf (.phase self))))
  (route self (.buffer self) (.buffer self)))



(defmethod osc-frame-value ((self sin-osc))
  (sin (* (/ (* 2 pi (midino-to-freq (.note self))) *sample-rate*)
          (.phase self))))


(defmethod osc-frame-value ((self saw-osc))
  (* 0.3d0        ;TODO 音大きいのでとりあえずつけとく。本来はいらない？
     (- (* (mod (/ (* (.phase self) (midino-to-freq (.note self)))
                   *sample-rate*)
                1d0)
           2d0)
        1d0)))



(defmethod process ((self adsr) (connection midi-connection) midi-events frame)
  (flet ((midi-event (i on-or-off)
           (loop for x in midi-events
                   thereis (and (or (= (.event x) on-or-off)
                                    (and (= on-or-off +midi-event-off+)
                                         (= (.event x) +midi-cc+)
                                         (= (.note x) +midi-cc-all-notes-off+)))
                                (= (.frame x) (mod (+ frame i) *frames-per-buffer*))
                                x))))
    (loop with sec-per-frame = (/ 1.0d0 *sample-rate*)
          for i below *frames-per-buffer*
          for off-event = (midi-event i +midi-event-off+)
          for on-event = (midi-event i +midi-event-on+) 
          for gate = (or on-event
                         (and (not off-event)
                              (.last-gate self)))
          for current = (* sec-per-frame (.frame self))
          if on-event
            do (setf (.frame self) 0)
          do (let ((value (if gate
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
                                  (setf (.release-value self) (.last-value self))
                                  (setf (.release-time self) current))
                                (let ((elapsed (- current (.release-time self))))
                                  (if (< elapsed (.r self))
                                      (max (* (.release-value self)
                                              (- 1.0d0 (/ elapsed (.r self))))
                                           0.0d0)
                                      0.0d0))))))
               (setf (aref (.buffer self) i) value)
               (incf (.frame self))
               (setf (.last-gate self) gate)
               (setf (.last-value self) value))))
  (let ((buffer (.buffer self)))
    (route self buffer buffer)))


(defmethod process ((self gain) (connection audio-connection) left right)
  (loop for i below *frames-per-buffer*
        with volume = (.volume self)
        do (setf (aref (.left self) i) (* (aref left i) volume))
           (setf (aref (.right self) i) (* (aref right i) volume)))
  (route self (.left self) (.right self))
  (loop for i below *frames-per-buffer*
        do (setf (aref (.left self) i) 0.0d0
                 (aref (.right self) i) 0.0d0)))

(defmethod process ((self master) (connection audio-connection) left right)
  (loop for i below *frames-per-buffer*
        do (incf (aref (.left self) i) (aref left i))
           (incf (aref (.right self) i) (aref right i))))

(defconstant +plugin-command-instrument+ 1)
(defconstant +plugin-command-effect+ 2)
(defconstant +plugin-command-manage+ 3)
(defconstant +plugin-command-edit+ 4)
(defconstant +plugin-command-quit+ 5)
(defconstant +plugin-command-get-state+ 6)
(defconstant +plugin-command-set-state+ 7)
(defconstant +plugin-command-get-params+ 8)
(defconstant +plugin-command-set-param+ 9)



(defmethod initialize-instance :after ((self plugin-model) &key)
  (run-plugin-host self)
  (when (slot-boundp self 'plugin-state)
    (set-plugin-state self)))

(defmethod connect-param ((from plugin-model) (to plugin-model) index)
  )

(defmethod run-plugin-host ((self plugin-model))
  (when (slot-boundp self 'plugin-description)
    (setf (.host-process self)
          (sb-ext:run-program *plugin-host-exe*
                              (list
                               "--sample-rate" (format nil "~f" *sample-rate*)
                               "--buffer-size" (format nil "~d" *frames-per-buffer*)
                               "--plugin-name" (.name (.plugin-description self)))
                              :wait nil))
    (push (.host-process self) *plugin-processes*)
    (let ((pipe (sb-win32::create-named-pipe (format nil "~a~a" *plugin-host-pipe-name*
                                                     (sb-ext:process-pid (.host-process self)))
                                             sb-win32::pipe-access-duplex
                                             sb-win32::pipe-type-byte
                                             255 0 0 100 (cffi-sys::null-pointer))))
      (setf (.host-io self)
            (sb-sys:make-fd-stream pipe :input t :output t :element-type 'unsigned-byte)))
    (get-params self)))

(defmethod close ((self plugin-model) &key abort)
  (declare (ignore abort))
  (let ((io (.host-io self)))
    (when io
      (sb-thread:with-mutex ((.mutex self))
        (write-byte +plugin-command-quit+ io)
        (ignore-errors (force-output io))
        (ignore-errors (close io))
        (setf (.host-io self) nil))))
  (call-next-method))

(defmethod get-plugin-state ((self plugin-model))
  (let ((io (.host-io self)))
    (sb-thread:with-mutex ((.mutex self))
      (write-byte +plugin-command-get-state+ io)
      (force-output io)
      (let* ((len (+ (read-byte io)
                     (ash (read-byte io) 8)
                     (ash (read-byte io) 16)
                     (ash (read-byte io) 24)))
             (state (make-array len :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
        (read-sequence state io)
        (setf (.plugin-state self) state)))))

(defmethod process ((self plugin-model)
                    (connection param-connection)
                    value _)
  (set-param self (.param connection) (aref value 0)))

(defmethod set-plugin-state ((self plugin-model))
  (let* ((io (.host-io self))
         (state (.plugin-state self))
         (state-length (length state)))
    (sb-thread:with-mutex ((.mutex self))
      (write-byte +plugin-command-set-state+ io)
      (loop repeat 100
            until (ignore-errors (not (force-output io)))
            do (sleep 0.1))
      (write-byte (logand state-length #xff) io)
      (write-byte (logand (ash state-length -8) #xff) io)
      (write-byte (logand (ash state-length -16) #xff) io)
      (write-byte (logand (ash state-length -24) #xff) io)
      (force-output io)
      (write-sequence state io)
      (force-output io))))


(declaim (inline receive-from-plugin))
(defun receive-from-plugin (nbuses io in left-buffer right-buffer)
  (declare (optimize (speed 3) (safety 0))
           ((simple-array (unsigned-byte 8) (*)) out in)
           (simple-vector left-buffer right-buffer))
  (loop for bus fixnum below nbuses
        do (loop for buffer in (list (aref left-buffer bus)
                                     (aref right-buffer bus))
                 do (read-sequence in io)
                    (locally
                        (declare ((SIMPLE-ARRAY DOUBLE-FLOAT (*)) buffer))
                      (loop for j fixnum below *frames-per-buffer*
                            with i fixnum = -1
                            do (setf (aref buffer j)
                                     (coerce (ieee-floats:decode-float32
                                              (let ((value 0))
                                                (setf (ldb (byte 8 0) value) (aref in (incf i)))
                                                (setf (ldb (byte 8 8) value) (aref in (incf i)))
                                                (setf (ldb (byte 8 16) value) (aref in (incf i)))
                                                (setf (ldb (byte 8 24) value) (aref in (incf i)))
                                                value))
                                             'double-float)))))))

(defmethod process ((self instrument-plugin-model)
                    (connection midi-connection)
                    midi-events frame)
  (declare (optimize (speed 3) (safety 0)))
  (let ((i -1)
        (length (length (the list midi-events)))
        (out (.out-buffer self))
        (in (.in-buffer self))
        (io (.host-io self))
        (left-buffer (.left-buffer self))
        (right-buffer (.right-buffer self)))
    (declare (fixnum i)
             ((simple-array (unsigned-byte 8) (*)) out in)
             (simple-vector left-buffer right-buffer))
    (setf (aref out (incf i)) +plugin-command-instrument+)
    (let ((bpm
            (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
              (ieee-floats:encode-float64
               (the double-float (.bpm (.sequencer *audio*))))))
          (nframes (current-frame (.sequencer *audio*))))
      (declare (fixnum nframes))
      (setf (aref out (incf i)) (if (playing) 1 0))
      (setf (aref out (incf i)) (ldb (byte 8 0) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 8) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 16) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 24) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 32) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 40) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 48) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 56) bpm))
      (setf (aref out (incf i)) (ldb (byte 8 0) nframes))
      (setf (aref out (incf i)) (ldb (byte 8 8) nframes))
      (setf (aref out (incf i)) (ldb (byte 8 16) nframes))
      (setf (aref out (incf i)) (ldb (byte 8 24) nframes))
      (setf (aref out (incf i)) (ldb (byte 8 32) nframes))
      (setf (aref out (incf i)) (ldb (byte 8 40) nframes))
      (setf (aref out (incf i)) (ldb (byte 8 48) nframes))
      (setf (aref out (incf i)) (ldb (byte 8 56) nframes)))
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
    (sb-thread:with-mutex ((.mutex self))
      (write-sequence out io :end (1+ i))
      (force-output io)
      (receive-from-plugin (.output-nbuses self) io in left-buffer right-buffer))
    (route self left-buffer right-buffer)))

(defmethod print-object ((self plugin-model) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a ~a"
            (.name (.plugin-description self))
            (.host-process self))))

(defun open-editor (plugin-model)
  (sb-thread:with-mutex ((.mutex plugin-model))
    (write-byte +plugin-command-edit+ (.host-io plugin-model))
    (force-output (.host-io plugin-model))))



(defun load-known-plugins ()
  (let ((xml (cxml:parse-file (format nil "~a\\CoLiTrSynth\\Plugin Host.settings"
                                      (sb-ext:posix-getenv "APPDATA"))
                              (stp:make-builder)))
        (plugin-descriptions))
    (xpath:do-node-set (node (xpath:evaluate "/PROPERTIES/VALUE[@name=\"pluginList\"]/KNOWNPLUGINS/*" xml))
      (push (make-instance
             'plugin-description
             :name (xpath:string-value (xpath:evaluate "@name" node))
             :format (xpath:string-value (xpath:evaluate "@format" node))
             :category (xpath:string-value (xpath:evaluate "@category" node))
             :manufacturer (xpath:string-value (xpath:evaluate "@manufacturer" node))
             :version (xpath:string-value (xpath:evaluate "@version" node))
             :file (xpath:string-value (xpath:evaluate "@file" node))
             :unique-id (xpath:string-value (xpath:evaluate "@uniqueId" node))
             :is-instrument (equal (xpath:string-value (xpath:evaluate "@isInstrument" node))
                                   "1")
             :num-inputs (xpath:string-value (xpath:evaluate "@numInputs" node))
             :num-outputs (xpath:string-value (xpath:evaluate "@numOutputs" node))
             :uid (xpath:string-value (xpath:evaluate "@uid" node)))
            plugin-descriptions))
    (loop for x in plugin-descriptions
          unless  (and (equal (.format x) "VST")
                       (some (lambda (y) (and (equal (.format y) "VST3")
                                              (equal (.name x) (.name y))))
                             plugin-descriptions))
            collect x)))

(defmethod get-params ((self plugin-model))
  (let ((io (.host-io self))
        (buffer (make-array 4 :element-type '(unsigned-byte 8)))
        (latency 0)
        (input-nbuses 0)
        (output-nbuses 0)
        (total-num-input-channels 0)
        (total-num-output-channels 0)
        (main-bus-num-input-channels 0)
        (main-bus-num-output-channels 0)
        (size 0))
    (sb-thread:with-mutex ((.mutex self))
      (write-byte +plugin-command-get-params+ io)
      (loop repeat 300
            until (ignore-errors (not (force-output io)))
            do (format *debug-io* "~&get-params: wait plugin host")
               (sleep 0.1))
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) latency) (aref buffer 0))
      (setf (ldb (byte 8 8) latency) (aref buffer 1))
      (setf (ldb (byte 8 16) latency) (aref buffer 2))
      (setf (ldb (byte 8 24) latency) (aref buffer 3))
      (setf (.latency self) latency)
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) input-nbuses) (aref buffer 0))
      (setf (ldb (byte 8 8) input-nbuses) (aref buffer 1))
      (setf (ldb (byte 8 16) input-nbuses) (aref buffer 2))
      (setf (ldb (byte 8 24) input-nbuses) (aref buffer 3))
      (setf (.input-nbuses self) input-nbuses)
      (setf (.out-buffer self)
            ;; 4 sizeof(float) 2 left+right
            (make-array
             (if (zerop input-nbuses)   ;MIDI のみ
                 1024                   ;サイズ適当
                 (* *frames-per-buffer* 4 2 input-nbuses))
             :element-type '(unsigned-byte 8)))
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) output-nbuses) (aref buffer 0))
      (setf (ldb (byte 8 8) output-nbuses) (aref buffer 1))
      (setf (ldb (byte 8 16) output-nbuses) (aref buffer 2))
      (setf (ldb (byte 8 24) output-nbuses) (aref buffer 3))
      (setf (.output-nbuses self) output-nbuses)
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) total-num-input-channels) (aref buffer 0))
      (setf (ldb (byte 8 8) total-num-input-channels) (aref buffer 1))
      (setf (ldb (byte 8 16) total-num-input-channels) (aref buffer 2))
      (setf (ldb (byte 8 24) total-num-input-channels) (aref buffer 3))
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) total-num-output-channels) (aref buffer 0))
      (setf (ldb (byte 8 8) total-num-output-channels) (aref buffer 1))
      (setf (ldb (byte 8 16) total-num-output-channels) (aref buffer 2))
      (setf (ldb (byte 8 24) total-num-output-channels) (aref buffer 3))
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) main-bus-num-input-channels) (aref buffer 0))
      (setf (ldb (byte 8 8) main-bus-num-input-channels) (aref buffer 1))
      (setf (ldb (byte 8 16) main-bus-num-input-channels) (aref buffer 2))
      (setf (ldb (byte 8 24) main-bus-num-input-channels) (aref buffer 3))
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) main-bus-num-output-channels) (aref buffer 0))
      (setf (ldb (byte 8 8) main-bus-num-output-channels) (aref buffer 1))
      (setf (ldb (byte 8 16) main-bus-num-output-channels) (aref buffer 2))
      (setf (ldb (byte 8 24) main-bus-num-output-channels) (aref buffer 3))
      (setf (.input-nchannels self) main-bus-num-input-channels)
      (setf (.sidechain-nchannels self) (- total-num-input-channels main-bus-num-input-channels))
      (setf (.output-nchannels self) total-num-output-channels)
      (read-sequence buffer io)
      (setf (ldb (byte 8 0) size) (aref buffer 0))
      (setf (ldb (byte 8 8) size) (aref buffer 1))
      (setf (ldb (byte 8 16) size) (aref buffer 2))
      (setf (ldb (byte 8 24) size) (aref buffer 3))
      (setf buffer (make-array size :element-type '(unsigned-byte 8)))
      (read-sequence buffer io))
    (with-input-from-string (in (sb-ext:octets-to-string buffer :external-format :utf-8))
      (setf (.params self)
            (loop for param in (read in)
                  collect (make-plugin-parameter :index (nth 0 param)
                                                 :name (nth 1 param)
                                                 :value (nth 2 param)
                                                 :value-as-text (nth 3 param))))))
  (setf (.left-buffer self) (make-array (.output-nbuses self)))
  (setf (.right-buffer self) (make-array (.output-nbuses self)))
  (loop for i below (.output-nbuses self)
        do (setf (aref (.left-buffer self) i) (make-buffer))
           (setf (aref (.right-buffer self) i) (make-buffer))))

(defmethod set-param ((self plugin-model) param value)
  (let ((io (.host-io self))
        (buffer (make-array 8 :element-type '(unsigned-byte 8)))
        (i -1)
        (index (plugin-parameter-index param)))
    (sb-thread:with-mutex ((.mutex self))
      (write-byte +plugin-command-set-param+ io)
      (force-output io)
      (setf (aref buffer (incf i)) (ldb (byte 8 0) index))
      (setf (aref buffer (incf i)) (ldb (byte 8 8) index))
      (setf (aref buffer (incf i)) (ldb (byte 8 16) index))
      (setf (aref buffer (incf i)) (ldb (byte 8 24) index))
      (let ((value (ieee-floats:encode-float32 value)))
        (setf (aref buffer (incf i)) (ldb (byte 8 0) value))
        (setf (aref buffer (incf i)) (ldb (byte 8 8) value))
        (setf (aref buffer (incf i)) (ldb (byte 8 16) value))
        (setf (aref buffer (incf i)) (ldb (byte 8 24) value)))
      (write-sequence buffer io)
      (force-output io))))
