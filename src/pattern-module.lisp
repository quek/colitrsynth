(in-package :colitrsynth)

(defmethod initialize-instance :after ((self pattern-module) &key)
  (let* ((pattern-editor (.pattern-editor self))
         (octave (make-instance 'label
                                :value (lambda ()
                                         (format nil "~d" (.octave pattern-editor)))
                                :x (- (.width self) (* *char-width* 4) *layout-space*)
                                :y *layout-space*))
         (edit-step (make-instance 'label
                                   :value (lambda ()
                                            (format nil "~2,'0d" (.edit-step pattern-editor)))
                                   :x (- (.width self) (* *char-width* 2) *layout-space*)
                                   :y *layout-space*)))
    (add-child self pattern-editor)
    (add-child self octave)
    (add-child self edit-step)
    (setf (.pattern pattern-editor) self
          (.x pattern-editor) *layout-space*
          (.y pattern-editor) (+ *font-size* (* *layout-space* 2))
          (.width pattern-editor) (- (.width self) 10)
          (.height pattern-editor) (- (.height self) (+ 15 *font-size*)))))

(defmethod render :before ((self pattern-editor-line) renderer)
  (let* ((editor (.parent self))
         (position (position self (.lines editor))))
    (setf (.value self)
          (with-output-to-string (out)
            (format out "~2,'0X" position)
            (loop with line = (.line self)
                  repeat (.length line)
                  for column across (.columns line)
                  for note = (.note column)
                  do (cond ((= note off)
                            (write-string " OFF --" out))
                           ((= note none)
                            (write-string " --- --" out))
                           (t
                            (let* ((c-s-o (format nil "~a" (midino-to-note note)))
                                   (c (char c-s-o 0))
                                   (s (if (char= (char c-s-o 1) #\#)
                                          #\#
                                          #\-))
                                   (o (char c-s-o (if (char= s #\#) 2 1))))
                              (format out " ~c~c~c ~2,'0X"
                                      c s o (.velocity column))))))))
    ;; play position
    (when (= (.current-line (.pattern editor)) position)
      (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
      (let ((play-x (.render-x self))
            (play-y (.render-y self))
            (play-w (.width self))
            (play-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect play-x play-y play-w play-h))))
    ;; selection
    (case (.selection-mode editor)
      (:line
       (when (or (<= (.selection-start editor)
                     position
                     (.cursor-y editor))
                 (<= (.cursor-y editor)
                     position
                     (.selection-start editor)))
         (apply #'sdl2:set-render-draw-color renderer *selection-color*)
         (let ((cursor-x (* *char-width* 3))
               (cursor-y (.render-y self))
               (cursor-w (* *char-width* +column-width+ (.length (.line self))))
               (cursor-h *char-height*))
           (sdl2:render-fill-rect
            renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))))))
    ;; cursor
    (when (= (.cursor-y editor) position)
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (let ((cursor-x (+ (* *char-width* (+ (.cursor-x editor) 3)) 2))
            (cursor-y (.render-y self))
            (cursor-w (if (zerop (mod (.cursor-x editor) +column-width+))
                          (* *char-width* 3)
                          *char-width*))
            (cursor-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))))))

(defmethod close ((self pattern-module) &key abort)
  (declare (ignore abort))
  (loop for track-view in (.tracks *sequencer-module*)
        do (loop for pattern-position-view in (.children track-view)
                 if (and (typep pattern-position-view 'pattern-position-view)
                         (eql self
                              (.pattern pattern-position-view)))
                   do (remove-pattern track-view pattern-position-view))))

(defmethod mousebuttondown :before ((self pattern-module) button state clicks x y)
  (setf (.selected-pattern *app*) self))

(defmethod keydown ((self pattern-module) value scancode mod-value)
  (unless (keydown (.pattern-editor self) value scancode mod-value)
    (call-next-method)))

(defmethod (setf .width) :after (value (self pattern-module))
  (setf (.width (.pattern-editor self)) (- (.width self) 10)))

(defmethod (setf .height) :after (value (self pattern-module))
  (setf (.height (.pattern-editor self)) (- (.height self) (+ 10 *font-size*))))

(defmethod serialize ((self pattern-module))
  `((setf (.length x) ,(.length self)
          (.lines x) ,(serialize (.lines self))
          (.current-line x) 0)
    (setf (.octave (.pattern-editor x)) ,(.octave (.pattern-editor self))
          (.edit-step (.pattern-editor x)) ,(.edit-step (.pattern-editor self)))
    ,@(call-next-method)))

(defmethod serialize ((self line))
  `(make-instance 'line
                  :columns ,(serialize (.columns self))
                  :lenght ,(.length self)))

(defmethod serialize ((self column))
  `(make-instance 'column
                  :note ,(.note self)
                  :velocity ,(.velocity self)))
