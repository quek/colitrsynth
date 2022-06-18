(in-package :colitrsynth)

(defmethod initialize-instance :after ((self pattern-editor-line) &key line index)
  (add-child self
             (setf (.index-label self)
                   (make-instance 'label :value (format nil "~2,'0X" index)
                                         :x 0 :y 0
                                         :width (* *char-width* 2)
                                         :height *char-height*)))
  (setf (.note-labels self)
        (loop for column across (.columns line)
              for x from 0
              collect (make-instance 'label :value "---"
                                            :x x :y 0
                                            :width (* *char-width* 3)
                                            :height *char-height*)))
  (setf (.velocity-labels self)
        (loop for column across (.columns line)
              for x from 0
              collect (make-instance 'label :value "--"
                                            :x x :y 0
                                            :width (* *char-width* 2)
                                            :height *char-height*)))
  (setf (.delay-labels self)
        (loop for column across (.columns line)
              for x from 0
              collect (make-instance 'label :value "--"
                                            :x x :y 0
                                            :width (* *char-width* 2)
                                            :height *char-height*))))

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
                            (write-string " OFF" out))
                           ((= note none)
                            (write-string " ---" out))
                           (t
                            (let* ((c-s-o (format nil "~a" (midino-to-note note)))
                                   (c (char c-s-o 0))
                                   (s (if (char= (char c-s-o 1) #\#)
                                          #\#
                                          #\-))
                                   (o (char c-s-o (if (char= s #\#) 2 1))))
                              (format out " ~c~c~c" c s o))))
                     (when (velocity-enable-p column)
                       (if (<= c0 note)
                           (format out " ~2,'0X" (.velocity column))
                           (format out " --")))
                     (when (delay-enable-p column)
                       (if (or (<= c0 note)
                               (= off note))
                           (format out " ~2,'0X" (.delay column))
                           (format out " --"))))))
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
               (cursor-w (* *char-width* (max-cursor-x editor)))
               (cursor-h *char-height*))
           (sdl2:render-fill-rect
            renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))))))
    ;; cursor
    (when (= (.cursor-y editor) position)
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (let ((cursor-x (+ (* *char-width* (+ (.cursor-x editor) 3)) 2))
            (cursor-y (.render-y self))
            (cursor-w (if (at-note-column-p editor (.cursor-x editor))
                          (* *char-width* 3)
                          *char-width*))
            (cursor-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))))))
