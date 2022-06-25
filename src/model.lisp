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
          (make-array (.nlines self)
                      :initial-contents 
                      (loop repeat (.nlines self)
                            collect (make-instance 'line))))))

(defmethod extend-column ((self pattern))
  (setf (.ncolumns self)
        (min (1+ (.ncolumns self))
             16)))

(defmethod extend-line ((self pattern))
  (let* ((new-nlines (1+ (.nlines self)))
         (old-lines (.lines self))
         (array-length (length old-lines)))
    (when (< array-length new-nlines)
      (let ((new-lines (make-array (+ array-length 16))))
        (loop for i below array-length
              do (setf (aref new-lines i)
                       (aref old-lines i)))
        (loop for i from array-length below (length new-lines)
              do (setf (aref new-lines i)
                       (make-instance
                        'line
                        :columns
                        (make-array 16
                                    :initial-contents
                                    (loop repeat 16
                                          collect (make-instance 'column))))))
        (setf (.lines self) new-lines)))
    (setf (.nlines self) new-nlines)
    (setf (.cursor-y (.pattern-editor self)) (1- new-nlines))))

(defmethod shrink-column ((self pattern))
  (setf (.ncolumns self) (max 1 (1- (.ncolumns self)))))

(defmethod shrink-line ((self pattern))
  (when (< 1 (.nlines self))
    (decf (.nlines self))))

(defmethod process-in (self (connection builtin-param-connection) left right)
  (when (plusp (length left))
    (funcall (fdefinition `(setf ,(builtin-parameter-accessor (.param connection))))
             (aref left 0)
             self)))

(defmethod process-in (self (connection builtin-param-connection) (left null) right)
  ;; デフォルト値設定したいかも
  )

(defmethod process-in ((self midi-input-mixin)
                       (connection midi-connection)
                       midi-events frame)
  (declare (optimize (speed 3) (safety 0)))
  (setf (.midi-events self) (append (.midi-events self) midi-events))
  (setf (.midi-frame self) frame))

(defmethod process-out :after ((self midi-input-mixin))
  (setf (.midi-events self) nil))
