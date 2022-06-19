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
(defgeneric process (model connection left right))
(defgeneric process-in (model connection left right))
(defgeneric process-out (model))
(defgeneric route (self left right))
(defgeneric prepare-save (model))


(defmethod process ((self sequencer-module) (connection null) left right))
(defmethod process ((self pattern-module) (connection null) left right))

(defmethod process ((self model) connection left right)
  (declare (optimize (speed 3) (safety 0)))
  (process-in self connection left right)
  (when (<= (the fixnum (length (the list (.in self))))
            (the fixnum (incf (the fixnum (.in-count self)))))
    (process-out self)
    (setf (.in-count self) 0)))

(defmethod process-in ((self model) (connection null) left right))

(defmethod route ((self model) left right)
  (declare (optimize (speed 3) (safety 0)))
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

;; TODO tracker の MIDI 出力も統一したい
(defmethod route-connection ((connection midi-connection)
                             (src instrument-plugin-model)
                             dest
                             left right)
  (process dest connection (.output-midi-events src) nil))

(defmethod close ((self model) &key abort)
  (declare (ignore abort)))

(defmethod prepare-save ((self model)))



(defmethod .name ((self pattern-position))
  (.name (.pattern self)))

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
          do (process model nil nil nil)))


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

(defmethod extend-line ((self pattern))
  (let* ((new-length (1+ (.length self)))
         (old-lines (.lines self))
         (array-length (length old-lines)))
    (when (< array-length new-length)
      (let ((new-lines (make-array (+ array-length 16))))
        (loop for i below array-length
              do (setf (aref new-lines i)
                       (aref old-lines i)))
        (loop for i from array-length below (length new-lines)
              with first-line = (aref old-lines 0)
              with length = (.length first-line)
              with first-column = (aref (.columns first-line) 0)
              with velocity-enable-p = (velocity-enable-p first-column)
              with delay-enable-p = (delay-enable-p first-column)
              do (setf (aref new-lines i)
                       (make-instance
                        'line
                        :length length
                        :columns
                        (make-array
                         16
                         :initial-contents
                         (loop repeat 16
                               collect (make-instance 'column
                                                      :velocity-enable-p  velocity-enable-p
                                                      :delay-enable-p delay-enable-p))))))
        (setf (.lines self) new-lines)))
    (setf (.length self) new-length)))

(defmethod shrink-column ((self pattern))
  (loop with length = (max 1 (1- (.length (aref (.lines self) 0))))
        for line across (.lines self)
        do (setf (.length line) length)))

(defmethod shrink-line ((self pattern))
  (when (< 1 (.length self))
    (decf (.length self))))

(defun midi-events-at-line-frame (pattern-position start-line start-frame end-line end-frame)
  (setf (.current-line (.pattern pattern-position)) start-line)
  (let (events)
    (loop with lines = (.lines (.pattern pattern-position))
          with delay-unit = (/ (frames-per-line) #x100)
          for current-line from start-line below (- (.end pattern-position)
                                                    (.start pattern-position))
          for current-frame = start-frame then 0
          while (<= current-line end-line)
          do (loop with line = (aref lines current-line)
                   for column across (.columns line)
                   for i below (.length line)
                   for note = (.note column)
                   for last-note = (aref (.last-notes pattern-position) i)
                   for delay-frame = (* delay-unit (.delay column))
                   if (or (< start-line current-line end-line)
                          (if (= start-line current-line end-line)
                              (and (<= current-frame delay-frame)
                                   (< delay-frame end-frame))
                              (if (= start-line current-line)
                                  (<= current-frame delay-frame)
                                  (if (= current-line end-line)
                                      (< delay-frame end-frame)))))
                     do (let ((midi-frame (round (- delay-frame current-frame))))
                          (when (or (and (<= c0 note)
                                         (<= c0 last-note))
                                    (and (= note off) (<= c0 last-note)))
                            (push (make-instance 'midi-event :event +midi-event-off+
                                                             :note last-note
                                                             :velocity 0
                                                             :frame midi-frame)
                                  events))
                          (when (<= c0 note)
                            (push (make-instance 'midi-event :event +midi-event-on+
                                                             :note note
                                                             :velocity (.velocity column)
                                                             :frame midi-frame)
                                  events))
                          (when (/= note none)
                            (setf (aref (.last-notes pattern-position) i) note)))))
    (sort events (lambda (a b) (if (= (.frame a) (.frame b))
                                   (< (.event a) (.event b))
                                   (< (.frame a) (.frame b)))))))


(defmethod process-in (self (connection builtin-param-connection) left right)
  (funcall (fdefinition `(setf ,(builtin-parameter-accessor (.param connection))))
           ;; TODO 先頭の値でいい？ 平均とかの方がいい？
           (aref left 0)
           self))

(defmethod process-in ((self midi-input-mixin)
                       (connection midi-connection)
                       midi-events frame)
  (declare (optimize (speed 3) (safety 0)))
  (setf (.midi-events self) (append (.midi-events self) midi-events))
  (setf (.midi-frame self) frame))

(defmethod process-out :after ((self midi-input-mixin))
  (setf (.midi-events self) nil))
