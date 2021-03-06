(in-package :colitrsynth)

(defmethod initialize-instance :after ((self sequencer-module) &key)
  (let* ((play-button (make-instance 'button :label "▶" :x 5 :y *layout-space*))
         (loop-button (make-instance 'loop-button :x 30 :y *layout-space*
                                                  :sequencer self))
         (add-track-button (make-instance 'button :label "+track" :x 55 :y *layout-space*))
         (bpm (make-instance 'label
                             :value (lambda ()
                                      (format nil "BPM ~f" (.bpm self)))
                             :x 115 :y *layout-space*))
         (track-heads-view (make-instance 'track-heads-view))
         (partial-view (make-instance 'sequencer-partial-view
                                      :timeline  (make-instance 'sequencer-timeline-view
                                                                :sequencer self)
                                      :tracks-view (make-instance 'sequencer-tracks-view))))
    (add-child self play-button)
    (add-child self loop-button)
    (add-child self add-track-button)
    (add-child self bpm)
    (add-child self track-heads-view)
    (add-child self partial-view)
    (setf (.track-heads-view self) track-heads-view)
    (setf (.partial-view self) partial-view)
    (loop for track in (.tracks self)
          do (add-new-track-after self track))
    (defmethod click ((play-button (eql play-button)) (button (eql 1)) x y)
      (if (.playing *audio*)
          (stop)
          (play-with-key)))
    (defmethod click ((add-track-button (eql add-track-button)) (button (eql 1)) x y)
      (add-new-track self))
    (resized self)))

(defmethod initialize-instance :after ((self pattern-position-view) &key)
  (add-child self
             (make-instance 'label
                            :value (lambda () (.name self))
                            :x *layout-space*
                            :y *layout-space*)))

(defmethod initialize-instance :after ((self sequencer-partial-view) &key)
  (add-child self (.timeline self))
  (add-child self (.tracks-view self)))

(defmethod initialize-instance :after ((self loop-button) &key)
  (setf (.fill-color self)
        (if (.looping (.sequencer self))
            *loop-color*
            *background-color*)))

(defmethod delete-track ((self sequencer-module) track)
  (disconnect-all track)
  (setf (slot-value self 'tracks) (remove track (.tracks self)))
  (let ((view (.track-heads-view self)))
    (setf (.children view)
          (remove track (.children view) :key #'.track)))
  (let ((view (.tracks-view (.partial-view self))))
    (setf (.children view) (remove track (.children view)))))

(defmethod mousebuttondown ((self track-view)
                            (button (eql sdl2-ffi:+sdl-button-right+))
                            state clicks x y)
  (setf (.drag-state *app*)
        (make-instance 'drag-state :target self :button button
                                   :x x :y y :state state)))

(defun pixcel-to-line (pixcel)
  ;; 16 ラインにグリッド
  (* (round (/ pixcel *pixcel-per-line*) 16) 16))

(defun line-to-pixcel (line)
  (* *pixcel-per-line* line))


(defmethod click ((self track-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (or (call-next-method)
      (awhen (.selected-pattern *app*)
        (let* ((start (pixcel-to-line x))
               (end (+ start (.nlines it))))
          (when (every (lambda (x)
                         (or (<= end (.start x))
                             (<= (.end x) start)))
                       (.pattern-positions self))
            (add-pattern self it start end))))
      t))

(defmethod serialize ((self track-view))
  `((setf (.pattern-positions x) ,(serialize (.pattern-positions self))
          (.name x) ,(.name self)
          (solo-p x) ,(solo-p self)
          (mute-p x) ,(mute-p self))
    (loop for i in (.pattern-positions x)
          do (add-child x i))
    ,@(call-next-method)))



(defmethod click ((self pattern-position-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (loop for module in (.modules *app*)
        with pattern = (.pattern self)
        if (eq module pattern)
          do (setf (.selected-pattern *app*) module)
             (loop-finish)))

(defmethod click ((self pattern-position-view)
                  (button (eql sdl2-ffi:+sdl-button-right+))
                  x y)
  (remove-pattern (.parent-by-class self 'track-view) self)
  (call-next-method))

(defmethod drag ((self pattern-position-view) xrel yrel
                 (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((pixcel (+ (.x self) (.move-delta-x self) xrel))
         (line (pixcel-to-line pixcel))
         (rounded-pixcel (line-to-pixcel line)))
    (setf (.x self) rounded-pixcel
          (.move-delta-x self) (- pixcel rounded-pixcel))))

(defmethod drag-end ((self pattern-position) x y
                     (button (eql sdl2-ffi:+sdl-button-left+)))
  (setf (.move-delta-x self) 0)
  (call-next-method))

(defmethod drop ((self track-view) (pattern-position-view pattern-position-view) x y button)
  (let* ((pattern-position pattern-position-view)
         (delta (- (pixcel-to-line (.x pattern-position-view)) (.start pattern-position))))
    (incf (.start pattern-position) delta)
    (incf (.end pattern-position) delta)
    (update-sequencer-end *sequencer-module*)))


(defmethod serialize ((self pattern-position-view))
  `((setf (.start x) ,(.start self)
          (.end x) ,(.end self)
          (.pattern x) ,(serialize-ref (.pattern self) :accessor '.pattern))
    ,@(call-next-method)))


(defun x-to-rounded-line (x)
  (* (round (/ x *pixcel-per-line*) 4) 4))

(defmethod click ((self sequencer-timeline-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line x)))
    (setf (play-position-line (.play-position sequencer))
          line
          (play-position-line-frame (.play-position sequencer))
          0)))

(defmethod click ((self sequencer-timeline-view)
                  (button (eql sdl2-ffi:+sdl-button-right+))
                  x y)
  (let* ((sequencer (.sequencer self)))
    (setf (.loop-start-line sequencer) 0)
    (setf (.loop-end-line sequencer) 0)))

(defmethod click ((self loop-button) (button (eql 1)) x y)
  (setf (.fill-color self)
        (if (setf (.looping (.sequencer self))
                  (not (.looping (.sequencer self))))
            *loop-color*
            *background-color*)))

(defmethod drag-start ((self sequencer-timeline-view) x y
                       (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line x)))
    (setf (.loop-start-line sequencer) line)
    (setf (.loop-end-line sequencer) line)))

(defmethod drag ((self sequencer-timeline-view) xrel yrel
                 (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line (local-mouse-position self))))
    (setf (.loop-end-line sequencer) line)))

(defmethod drag-end ((self sequencer-timeline-view) x y
                     (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((sequencer (.sequencer self))
         (line (x-to-rounded-line x)))
    (setf (.loop-end-line sequencer) line)
    (when (< (.loop-end-line sequencer) (.loop-start-line sequencer))
      (rotatef (.loop-end-line sequencer) (.loop-start-line sequencer)))))

(defmethod render ((self sequencer-timeline-view) renderer)
  (let* ((sequencer (.sequencer self))
         (end-line (.end sequencer))
         (loop-start (.loop-start-line sequencer))
         (loop-end (.loop-end-line sequencer)))
    (when (/= loop-start loop-end)
      (apply #'sdl2:set-render-draw-color renderer *loop-color*)
      (sdl2:render-fill-rect renderer
                             (sdl2:make-rect
                              (+ (* *pixcel-per-line* loop-start)
                                 (.render-x self))
                              (.render-y self)
                              (+ (* *pixcel-per-line* (- loop-end loop-start)))
                              (.height self))))
    (apply #'sdl2:set-render-draw-color renderer *default-color*)
    (loop for i from 0 to (/ (+ end-line 16) 4)
          for x = (+ (* *pixcel-per-line* i 4 4 4) (.render-x self))
          do (sdl2:render-draw-line renderer
                                    x
                                    (+ (.render-y self) (floor (/ (.height self) 2)))
                                    x
                                    (+ (.render-y self) (.height self))))
    (call-next-method)))

(defmethod render :after ((self sequencer-partial-view) renderer)
  (let* ((sequencer (.root-parent self))
         (x (max (.render-x self)
                 (min
                  (+ (.render-x self)
                     (.width self))
                  (+ (.render-x self)
                     (* (play-position-line (.play-position sequencer))
                        *pixcel-per-line*)
                     (- (.offset-x self))))))
         (y (.render-y self)))
    (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
    (sdl2:render-draw-line renderer x y x (+ y (.height self)))))

(defmethod render ((self loop-button) render)
  (apply #'sdl2:set-render-draw-color render (.fill-color self))
  (sdl2:render-fill-rect render
                         (sdl2:make-rect (.render-x self)
                                         (.render-y self)
                                         (.width self)
                                         (.height self)))
  (call-next-method))

(defmethod keydown ((self sequencer-module) value scancode mod-value)
  (aif (gethash *current-key* *sequencer-keymap*)
       (progn
         (funcall it self)
         t)
       (call-next-method)))

(defmethod drag-start ((self sequencer-module) x y (button (eql 3)))
  "track-view にディスパッチする。"
  (awhen (child-view-at self x y)
    (drag-start it (- x (.x it)) (- y (.y it)) button)))

(defmethod add-new-track ((self sequencer-module))
  (let ((track (make-instance 'track-view
                              :name (format nil "Track~a"
                                            (1+ (length (.tracks self)))) )))
    (setf (slot-value self 'tracks)
          (append (.tracks self) (list track)))
    (add-new-track-after self track)
    (resized self)
    track))

(defmethod add-new-track-after ((self sequencer-module) (track-view track-view))
  (let* ((tracks-view (.tracks-view (.partial-view self)))
         (track-heads-view (.track-heads-view self))
         (track-head-view (make-instance 'track-head-view :track track-view)))
    (add-child tracks-view track-view)
    (add-child track-heads-view track-head-view))
  track-view)

(defmethod (setf .offset-y) :after (value (self track-heads-view))
  (let* ((sequencer (.parent-by-class self 'sequencer-module))
         (tracks-view (.tracks-view (.partial-view sequencer))))
    (setf (slot-value tracks-view 'offset-y) value)))

(defmethod (setf .offset-y) :after (value (self sequencer-tracks-view))
  (let* ((sequencer (.parent-by-class self 'sequencer-module))
         (track-heads-view (.track-heads-view sequencer)))
    (setf (slot-value track-heads-view 'offset-y) value)))


(defmethod serialize ((self sequencer-module))
  `((setf (.bpm x) ,(.bpm self)
          (.lpb x) ,(.lpb self)
          (.looping x) ,(.looping self))
    ,@(loop for track in (.tracks self)
            collect `(setf (gethash ,(ref-id track) *serialize-table*)
                           ,(serialize track)))
    (s (setf (.tracks x)
             (list ,@(loop for track in (.tracks self)
                           collect `(r ,(ref-id track))))))
    ,@(call-next-method)))

(defmethod (setf .tracks) :after (tracks (self sequencer-module))
  (loop for track in tracks
        do (add-new-track-after self track))
  (update-sequencer-end self))

(defmethod add-pattern ((track-view track-view) (pattern pattern-mixin) start end)
  (let ((pattern-position-view (make-instance 'pattern-position-view
                                              :pattern pattern
                                              :start start :end end)))
    (push pattern-position-view
          (.pattern-positions track-view))
    (update-sequencer-end *sequencer-module*)
    (add-child track-view pattern-position-view)
    (setf (.x pattern-position-view) (* *pixcel-per-line* (.start pattern-position-view))
          (.y pattern-position-view) 2
          (.width pattern-position-view) (* *pixcel-per-line* (- (.end pattern-position-view)
                                                                 (.start pattern-position-view)))
          (.height pattern-position-view) (- (.height track-view) 4))
    pattern-position-view))

(defmethod remove-pattern ((track-view track-view)
                           (pattern-position-view pattern-position-view))
  (setf (.pattern-positions track-view)
        (remove pattern-position-view (.pattern-positions track-view)))
  (update-sequencer-end *sequencer-module*)
  (remove-child track-view pattern-position-view))

(defmethod resized ((self track-heads-view))
  (let ((sequencer-module (.root-parent self)))
    (setf (.x self) *layout-space*)
    (setf (.y self) (+ 26 15))
    (setf (.width self) 100)
    (setf (.height self) (- (.height sequencer-module)
                            (.y self)
                            *layout-space*)))
  (call-next-method))

(defmethod resized ((self sequencer-partial-view))
  (let* ((sequencer-module (.root-parent self))
         (track-heads-view (.track-heads-view sequencer-module)))
    (setf (.x self) (+ (.x track-heads-view) (.width track-heads-view)))
    (setf (.y self) 26)
    (setf (.width self) (- (.width sequencer-module)
                           (.width track-heads-view)
                           (* *layout-space* 2)))
    (setf (.height self) (.height track-heads-view)))
  (call-next-method))

(defmethod resized ((self track-view))
  (let* ((parent (.parent self))
         (sequencer-module (.parent-by-class self 'sequencer-module))
         (index (position self (.tracks sequencer-module))))
    (setf (.x self) 0)
    (setf (.y self) (+ (* *track-height* index)))
    (setf (.height self) *track-height*)
    (setf (.width self) (max (.width parent) (* (+ 16 (.end sequencer-module))
                                                *pixcel-per-line*))))
  (call-next-method)
  (let ((connector (.connector self)))
    (setf (.x connector)
          (let ((partial-view (.parent-by-class self 'sequencer-partial-view)))
            (round (+ (/ (.width partial-view) 2)
                      (.offset-x partial-view)))))
    (setf (.y connector) (- (.height self) (.height connector)))))


(defmethod resized ((self pattern-position-view))
  (setf (.height self) (- (.height (.parent self)) 4)))

(defmethod resized ((self sequencer-timeline-view))
  (setf (.height self) 15)     ;width track-view の resized で設定する
  (let ((end-line (max (.end (.parent-by-class self 'sequencer-module))
                       (/ (.width self) *pixcel-per-line*))))
    (loop for i from (length (.labels self)) to (/ end-line 4)
          for label = (make-instance 'label :value (princ-to-string (1+ i))
                                            :x (+ (* *pixcel-per-line* i 4 4 4) 3))
          do (add-child self label)
             (push label (.labels self))))
  (call-next-method))

(defmethod resized ((self sequencer-tracks-view))
  (let* ((parent (.parent self))
         (timeline (.timeline parent)))
    (setf (.x self) 0)
    (setf (.y self) (.height timeline))
    (setf (.width self) (multiple-value-bind (x1 y1 x2 y2)
                            (children-bounds self)
                          (declare (ignore x1 y1 y2))
                          x2))
    (setf (.height self) (- (.height parent) (.height timeline))))
  (call-next-method))

(defmethod wheel ((self sequencer-partial-view) delta)
  (if (.shift-key-p *app*)
      (call-next-method)
      (awhen (child-view-at self
                            (- (.mouse-x *app*) (.render-x self))
                            (- (.mouse-y *app*) (.render-y self)))
        (wheel it delta))))
