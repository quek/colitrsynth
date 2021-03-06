(in-package :colitrsynth)

(defmethod initialize-instance :after ((self pattern-module) &key)
  (let* ((editor (.editor self))
         (octave (make-instance 'label
                                :value (lambda ()
                                         (format nil "~d" (.octave editor)))
                                :x (- (.width self) (* *char-width* 4) *layout-space*)
                                :y *layout-space*))
         (edit-step (make-instance 'label
                                   :value (lambda ()
                                            (format nil "~2,'0d" (.edit-step editor)))
                                   :x (- (.width self) (* *char-width* 2) *layout-space*)
                                   :y *layout-space*)))
    (add-child self editor)
    (add-child self octave)
    (add-child self edit-step)
    (setf (.model editor) self
          (.x editor) *layout-space*
          (.y editor) (+ *font-size* (* *layout-space* 2))
          (.width editor) (- (.width self) 10)
          (.height editor) (- (.height self) (+ 15 *font-size*)))))

(defmethod close ((self pattern-module) &key abort)
  (declare (ignore abort))
  (loop for track-view in (.tracks *sequencer-module*)
        do (loop for pattern-position-view in (.children track-view)
                 if (and (typep pattern-position-view 'pattern-position-view)
                         (eql self
                              (.pattern pattern-position-view)))
                   do (remove-pattern track-view pattern-position-view))))

(defmethod delay-enable-p ((self pattern-module) column)
  (aref (.delay-enables self) column))

(defmethod (setf delay-enable-p) (value (self pattern-module) column)
  (setf (aref (.delay-enables self) column) value))

(defmethod events-at-line-frame ((pattern pattern) pattern-position start-line start-frame end-line end-frame)
  (let (events)
    (loop with lines = (.lines pattern)
          with delay-unit = (/ (frames-per-line) #x100)
          for current-line from start-line below (- (.end pattern-position)
                                                    (.start pattern-position))
          for current-frame = start-frame then 0
          while (<= current-line end-line)
          do (loop with line = (aref lines current-line)
                   for column across (.columns line)
                   for i below (.ncolumns pattern)
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
                          (when (or (and (valid-note-p note)
                                         (valid-note-p last-note))
                                    (and (= note off) (valid-note-p last-note)))
                            (push (make-instance 'midi-event :event +midi-event-off+
                                                             :note last-note
                                                             :velocity 0
                                                             :frame midi-frame)
                                  events))
                          (when (valid-note-p note)
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

(defmethod mousebuttondown :before ((self pattern-mixin) button state clicks x y)
  (setf (.selected-pattern *app*) self))

(defmethod keydown ((self pattern-module) value scancode mod-value)
  (unless (keydown (.editor self) value scancode mod-value)
    (call-next-method)))

(defmethod (setf .width) :after (value (self pattern-module))
  (setf (.width (.editor self)) (- (.width self) 10)))

(defmethod (setf .height) :after (value (self pattern-module))
  (setf (.height (.editor self)) (- (.height self) (+ 10 *font-size*))))

(defmethod process ((self pattern-mixin) (connection null) left right))

(defmethod serialize ((self pattern-mixin))
  `((setf (.nlines x) ,(.nlines self)
          (.lines x) ,(serialize (.lines self))
          (.current-line x) 0)
    ,@(call-next-method)))

(defmethod serialize ((self pattern-module))
  `((setf (.ncolumns x) ,(.ncolumns self)
          (.velocity-enables x) ,(.velocity-enables self)
          (.delay-enables x) ,(.delay-enables self))
    (setf (.octave (.editor x)) ,(.octave (.editor self))
          (.edit-step (.editor x)) ,(.edit-step (.editor self)))
    ,@(call-next-method)))

(defmethod velocity-enable-p ((self pattern-module) column)
  (aref (.velocity-enables self) column))

(defmethod (setf velocity-enable-p) (value (self pattern-module) column)
  (setf (aref (.velocity-enables self) column) value))
