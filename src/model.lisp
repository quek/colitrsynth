(in-package :colitrsynth)

(defparameter *transparency* #xc0)

(defgeneric initialize (x))
(defgeneric connect (in out))
(defgeneric disconnect (in out))
(defgeneric disconnect-all (self))
(defgeneric process (model left right))
(defgeneric route (self left right))
(defgeneric prepare-save (model))

;; これも保存したいのでここに書く
(defclass renderable ()
  ((color :initarg :color :initform (list #xdd #xdd #xdd *transparency*) :accessor .color)
   (x :initarg :x :initform 0 :accessor .x)
   (y :initarg :y :initform 0 :accessor .y)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)))

(defclass name-mixin ()
  ((name :initarg :name :initform "" :accessor .name)))

(defclass model (name-mixin renderable)
  ((in :initarg :in :accessor .in :initform nil)
   (out :initarg :out :accessor .out :initform nil)))

(defmethod initialize-instance :after ((self model) &key)
  (initialize self))

(defmethod initialize ((self model)))

(defmethod connect ((in model) (out model))
  (push out (.out in))
  (push in (.in out)))

(defmethod disconnect ((in model) (out model))
  (setf (.out in) (remove out (.out in)))
  (setf (.in out) (remove in (.in out))))

(defmethod disconnect-all ((self model))
  (loop for in in (copy-list (.in self))
        do (disconnect in self))
  (loop for out in (copy-list (.out self))
        do (disconnect self out)))

(defmethod route ((self model) left right)
  (loop for out in (.out self)
        do (process out left right)))

(defmethod close ((self model) &key abort)
  (declare (ignore abort)))

(defmethod prepare-save ((self model)))

(defclass pattern-position ()
  ((pattern :initarg :pattern :accessor .pattern)
   (start :initarg :start :accessor .start)
   (end :initarg :end :accessor .end)
   (last-notes :accessor .last-notes
               :initform (make-array 16 :initial-element off))))

(defmethod .name ((self pattern-position))
  (.name (.pattern self)))

(defclass track (model)
  ((pattern-positions :initform nil :accessor .pattern-positions)
   (buffer :accessor .buffer)))

(defmethod initialize ((self track))
  (setf (.buffer self) (make-buffer :initial-element nil :element-type t)))

(defmethod lepis:emit-slot ((self track) (slot (eql 'buffer)) stream)
  (format stream " NIL")
  nil)

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
                                (midi-events-at-line-frame pattern-position
                                                           (- start-line start) start-frame
                                                           (- end-line start) end-frame)))))))
    (when midi-events
      (print midi-events))
    (route track midi-events start-frame)))

(defun play-track-all-off (track start-frame)
  (route track (print (list (midi-event-all-notes-off))) start-frame))

(defun play-track-no-notes (track start-frame)
  (route track nil start-frame))

(defclass sequencer (model)
  ((bpm :initarg :bpm :initform 90.0d0 :accessor .bpm
        :type double-float)
   (lpb :initarg :lpb :initform 4 :accessor .lpb)
   (tracks :initarg :tracks :accessor .tracks :initform nil)
   (end :initform 0 :accessor .end)
   (looping :initform t :accessor .looping)
   (current-line :initform 0 :accessor .current-line))
  (:default-initargs :color (list #x00 #xff #xff *transparency*)
                     :x 5 :y 5
                     :width 700
                     :height 200))

(defmethod add-new-track ((self sequencer))
  (let ((track (make-instance 'track)))
    (setf (.tracks self)
          (append (.tracks self) (list track)))
    track))

(defun update-sequencer-end ()
  (let ((sequencer (.sequencer *audio*)))
    (setf (.end sequencer)
          (loop for track in (.tracks sequencer)
                maximize (loop for pattern-position in (.pattern-positions track)
                               maximize (.end pattern-position))))))

(defun process-sequencer (self start-line start-frame end-line end-frame)
  (let ((end (.end self)))
    (if (or (zerop end)
            (< end start-line))
       (progn
         (loop for track in (.tracks self)
               do (play-track-no-notes track start-frame))
         (write-master-buffer)
         (setf (.current-line self) end))
       (progn
         (cond ((playing)
                (loop for track in (.tracks self)
                      do (play-track track start-line start-frame end-line end-frame)))
               ((played)
                (loop for track in (.tracks self)
                      do (play-track-all-off track start-frame)))
               (t
                (loop for track in (.tracks self)
                      do (play-track-no-notes track start-frame))))
         (write-master-buffer)
         (setf (.current-line self) start-line)))))

(defclass column ()
  ((note :initarg :note :initform none :accessor .note)
   (velocity :initarg :velocity :initform 100 :accessor .velocity)))

(defclass line ()
  ((columns :initarg :columns :accessor .columns
            :initform (make-array 16 :initial-contents
                                  (loop repeat 16 collect (make-instance 'column))))
   (length :initarg :lenght :initform 1 :accessor .length)))

(defclass pattern (model)
  ((length :initarg :length :initform #x20 :accessor .length)
   (lines :initarg :lines :accessor .lines)
   (current-line :initform 0 :accessor .current-line))
  (:default-initargs :name "Pattern" :height 300))

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

(defmethod remove-pattern ((track track) (pattern-position pattern-position))
  (setf (.pattern-positions track)
        (remove pattern-position (.pattern-positions track)))
  (update-sequencer-end))

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

(defclass osc (model)
  ((note :initarg :note :initform off :accessor .note)
   (buffer :accessor .buffer)
   (value :initform 0.0d0 :accessor .value)
   (phase :initform 0.0d0 :accessor .phase
          :type double-float)))

(defmethod initialize ((self osc))
  (setf (.buffer self) (make-buffer)))

(defmethod lepis:emit-slot ((self osc) (slot (eql 'buffer)) stream)
  (format stream " NIL")
  nil)

(defmethod process ((self osc) midi-events frame)
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

(defclass sin-osc (osc)
  ()
  (:default-initargs :name "Sin"))

(defmethod osc-frame-value ((self sin-osc))
  (sin (* (/ (* 2 pi (midino-to-freq (.note self))) *sample-rate*)
          (.phase self))))

(defclass saw-osc (osc)
  ()
  (:default-initargs :name "Saw"))

(defmethod osc-frame-value ((self saw-osc))
  (* 0.3d0        ;TODO 音大きいのでとりあえずつけとく。本来はいらない？
     (- (* (mod (/ (* (.phase self) (midino-to-freq (.note self)))
                   *sample-rate*)
                1d0)
           2d0)
        1d0)))

(defclass adsr (model)
  ((a :initarg :a :initform 0.003d0 :accessor .a)
   (d :initarg :d :initform 0.05d0 :accessor .d)
   (s :initarg :s :initform 0.3d0 :accessor .s)
   (r :initarg :r :initform 0.1d0 :accessor .r)
   (buffer :accessor .buffer)
   (last-gate :initform nil :accessor .last-gate)
   (last-value :initform 0.0d0 :accessor .last-value)
   (frame :initform 0 :accessor .frame)
   (release-time :initform 0.0d0 :accessor .release-time)
   (release-value :initform 0.0d0 :accessor .release-value))
  (:default-initargs :name "Adsr" :height 95))

(defmethod initialize ((self adsr))
  (setf (.buffer self) (make-buffer)))

(defmethod lepis:emit-slot ((self adsr) (slot (eql 'buffer)) stream)
  (format stream " NIL")
  nil)


(defmethod process ((self adsr) midi-events frame)
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

(defclass amp (model)
  ((left :accessor .left)
   (right :accessor .right)
   (in-count :initform 0 :accessor .in-count))
  (:default-initargs :name "Amp"))

(defmethod initialize ((self amp))
  (setf (.left self) (make-buffer))
  (setf (.right self) (make-buffer)))

(defmethod lepis:emit-slot ((self amp) (slot (eql 'left)) stream)
  (format stream " NIL")
  nil)

(defmethod lepis:emit-slot ((self amp) (slot (eql 'right)) stream)
  (format stream " NIL")
  nil)

(defmethod process ((self amp) left right)
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

(defclass master (model)
  ((left :accessor .left)
   (right :accessor .right)
   (volume :initform 0.6d0 :accessor .volume))
  (:default-initargs  :name "Master" :x 695 :y 515
                      :color (list #xff #xa5 #x00 *transparency*)))

(defmethod initialize ((self master))
  (setf (.left self) (make-buffer))
  (setf (.right self) (make-buffer)))

(defmethod lepis:emit-slot ((self master) (slot (eql 'left)) stream)
  (format stream " NIL")
  nil)

(defmethod lepis:emit-slot ((self master) (slot (eql 'right)) stream)
  (format stream " NIL")
  nil)

(defmethod process ((self master) left right)
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

(defclass plugin-model (model)
  ((plugin-description :initarg :plugin-description :accessor .plugin-description)
   (host-process :accessor .host-process)
   (host-io :accessor .host-io)
   (out-buffer :accessor .out-buffer)
   (in-buffer :accessor .in-buffer)
   (left-buffer :accessor .left-buffer)
   (right-buffer :accessor .right-buffer)
   (plugin-state :accessor .plugin-state)
   (mutex :accessor .mutex)))

(defmethod initialize ((self plugin-model))
  (setf (.out-buffer self) (make-array (* *frames-per-buffer* 9) :element-type 'unsigned-byte))
  (setf (.in-buffer self) (make-array (* *frames-per-buffer* 4) :element-type 'unsigned-byte))
  (setf (.left-buffer self) (make-buffer))
  (setf (.right-buffer self) (make-buffer))
  (setf (.mutex self) (sb-thread:make-mutex))
  (run-plugin-host self)
  (when (slot-boundp self 'plugin-state)
    (set-plugin-state self)))

(defmethod process ((self plugin-model) x y)
  (when (.host-io self)
    (call-next-method)))

(defmethod run-plugin-host ((self plugin-model))
  (setf (.host-process self)
        (sb-ext:run-program *plugin-host-exe*
                            (list (.name (.plugin-description self)))
                            :wait nil))
  (let ((pipe (sb-win32::create-named-pipe (format nil "~a~a" *plugin-host-pipe-name*
                                                   (sb-ext:process-pid (.host-process self)))
                                           sb-win32::pipe-access-duplex
                                           sb-win32::pipe-type-byte
                                           255 0 0 100 (cffi-sys::null-pointer))))
    (setf (.host-io self)
          (sb-sys:make-fd-stream pipe :input t :output t :element-type 'unsigned-byte))))

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

(defmethod prepare-save ((self plugin-model))
  (get-plugin-state self))

(defmethod lepis:emit-slot ((self plugin-model) (slot (eql 'out-buffer)) stream)
  (format stream " NIL")
  nil)

(defmethod lepis:emit-slot ((self plugin-model) (slot (eql 'in-buffer)) stream)
  (format stream " NIL")
  nil)

(defmethod lepis:emit-slot ((self plugin-model) (slot (eql 'left-buffer)) stream)
  (format stream " NIL")
  nil)

(defmethod lepis:emit-slot ((self plugin-model) (slot (eql 'right-buffer)) stream)
  (format stream " NIL")
  nil)

(defclass instrument-plugin-model (plugin-model) ())
(defclass effect-plugin-model (plugin-model) ())

(defmethod process ((self instrument-plugin-model) midi-events frame)
  (let ((i -1)
        (length (length midi-events))
        (out (.out-buffer self))
        (in (.in-buffer self))
        (io (.host-io self))
        (left-buffer (.left-buffer self))
        (right-buffer (.right-buffer self)))
    (setf (aref out (incf i)) +plugin-command-instrument+)
    (let ((bpm (ieee-floats:encode-float64 (.bpm (.sequencer *audio*))))
          (nframes (.nframes *audio*)))
      (setf (aref out (incf i)) (if (playing) 1 0))
      (setf (aref out (incf i)) (mod bpm #x100))
      (setf (aref out (incf i)) (mod (ash bpm -8) #x100))
      (setf (aref out (incf i)) (mod (ash bpm -16) #x100))
      (setf (aref out (incf i)) (mod (ash bpm -24) #x100))
      (setf (aref out (incf i)) (mod (ash bpm -32) #x100))
      (setf (aref out (incf i)) (mod (ash bpm -40) #x100))
      (setf (aref out (incf i)) (mod (ash bpm -48) #x100))
      (setf (aref out (incf i)) (mod (ash bpm -56) #x100))
      (setf (aref out (incf i)) (mod nframes #x100))
      (setf (aref out (incf i)) (mod (ash nframes -8) #x100))
      (setf (aref out (incf i)) (mod (ash nframes -16) #x100))
      (setf (aref out (incf i)) (mod (ash nframes -24) #x100))
      (setf (aref out (incf i)) (mod (ash nframes -32) #x100))
      (setf (aref out (incf i)) (mod (ash nframes -40) #x100))
      (setf (aref out (incf i)) (mod (ash nframes -48) #x100))
      (setf (aref out (incf i)) (mod (ash nframes -56) #x100)))
    (setf (aref out (incf i)) (mod length #x100))
    (setf (aref out (incf i)) (mod (ash length -8) #x100))
    (loop for midi-event in midi-events
          do (setf (aref out (incf i)) (.event midi-event))
             (setf (aref out (incf i)) (.channel midi-event))
             (setf (aref out (incf i)) (.note midi-event))
             (setf (aref out (incf i)) (.velocity midi-event))
             (setf (aref out (incf i)) (mod (.frame midi-event) #x100))
             (setf (aref out (incf i)) (mod (ash (.frame midi-event) -8) #x100)))
    (sb-thread:with-mutex ((.mutex self))
      (write-sequence out io :end (1+ i))
      (force-output io)
      (loop for buffer in (list left-buffer right-buffer)
            do (let ((position (read-sequence in io)))
                 (declare (ignore position))
                 (loop for i below *frames-per-buffer*
                       do (setf (aref buffer i)
                                (coerce (ieee-floats:decode-float32
                                         (+ (aref in (* 4 i))
                                            (ash (aref in (+ (* 4 i) 1)) 8)
                                            (ash (aref in (+ (* 4 i) 2)) 16)
                                            (ash  (aref in (+ (* 4 i) 3)) 24)))
                                        'double-float))))))
    (route self left-buffer right-buffer)))

(defmethod process ((self effect-plugin-model) left right)
  (let ((out (.out-buffer self))
        (in (.in-buffer self))
        (io (.host-io self))
        (left-buffer (.left-buffer self))
        (right-buffer (.right-buffer self)))
    (loop for lr in (list left right)
          with j = -1
          do (loop for i below *frames-per-buffer*
                   for n = (ieee-floats:encode-float32 (aref lr i))
                   do (setf (aref out (incf j)) (mod n #x100))
                      (setf (aref out (incf j)) (mod (ash n -8) #x100))
                      (setf (aref out (incf j)) (mod (ash n -16) #x100))
                      (setf (aref out (incf j)) (mod (ash n -24) #x100))))
    (sb-thread:with-mutex ((.mutex self))
      (write-byte +plugin-command-effect+ io)
      (let ((bpm (ieee-floats:encode-float64 (.bpm (.sequencer *audio*))))
            (nframes (.nframes *audio*)))
        (write-byte (if (playing) 1 0) io)
        (write-byte (mod bpm #x100) io)
        (write-byte (mod (ash bpm -8) #x100) io)
        (write-byte (mod (ash bpm -16) #x100) io)
        (write-byte (mod (ash bpm -24) #x100) io)
        (write-byte (mod (ash bpm -32) #x100) io)
        (write-byte (mod (ash bpm -40) #x100) io)
        (write-byte (mod (ash bpm -48) #x100) io)
        (write-byte (mod (ash bpm -56) #x100) io)
        (write-byte (mod nframes #x100) io)
        (write-byte (mod (ash nframes -8) #x100) io)
        (write-byte (mod (ash nframes -16) #x100) io)
        (write-byte (mod (ash nframes -24) #x100) io)
        (write-byte (mod (ash nframes -32) #x100) io)
        (write-byte (mod (ash nframes -40) #x100) io)
        (write-byte (mod (ash nframes -48) #x100) io)
        (write-byte (mod (ash nframes -56) #x100) io))
      ;; TODO バッファの一部のみ read, write される対応
      ;; ここの force-output がないと write-byte したのが write-sequence した分と一緒に
      ;; 1 + 4095, 4096 とリードされ 1 バイト分読まれずに残ってしまう。
      (force-output io)
      (write-sequence out io :end (* *frames-per-buffer* 4 2))
      (force-output io)
      (loop for buffer in (list left-buffer right-buffer)
            do (let ((position (read-sequence in io :end (* *frames-per-buffer* 4))))
                 (declare (ignore position))
                 (loop for i below *frames-per-buffer*
                       do (setf (aref buffer i)
                                (coerce (ieee-floats:decode-float32
                                         (+ (aref in (* 4 i))
                                            (ash (aref in (+ (* 4 i) 1)) 8)
                                            (ash (aref in (+ (* 4 i) 2)) 16)
                                            (ash  (aref in (+ (* 4 i) 3)) 24)))
                                        'double-float))))))
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

(defclass plugin-description ()
  ((name :initarg :name :accessor .name)
   (format :initarg :format :accessor .format)
   (category :initarg :category :accessor .category)
   (manufacturer :initarg :manufacturer :accessor .manufacturer)
   (version :initarg :version :accessor .version)
   (file :initarg :file :accessor .file)
   (unique-id :initarg :unique-id :accessor .unique-id)
   (is-instrument :initarg :is-instrument :accessor .is-instrument)
   (num-inputs :initarg :num-inputs :accessor .num-inputs)
   (num-outputs :initarg :num-outputs :accessor .num-outputs)
   (uid :initarg :uid :accessor .uid)))

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


(defmethod lepis:emit ((self stream) stream sharp-dot)
  (print nil stream))

(defmethod lepis:emit ((self sb-impl::process) stream sharp-dot)
  (print nil stream))

(defmethod lepis:emit ((self sb-thread:mutex) stream sharp-dot)
    (print nil stream))

(defmethod lepis::store-object ((object stream) hash done)
  (sunless (gethash object done)
    (setf it t)
    (setf (gethash object hash) (hash-table-count hash))))

(defmethod lepis::store-object ((object sb-impl::process) hash done)
  (sunless (gethash object done)
    (setf it t)
    (setf (gethash object hash) (hash-table-count hash))))

(defmethod lepis::store-object ((object sb-thread::mutex) hash done)
  (sunless (gethash object done)
    (setf it t)
    (setf (gethash object hash) (hash-table-count hash))))
