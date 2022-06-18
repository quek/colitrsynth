(in-package :colitrsynth)

(defmethod initialize-instance :after ((self pattern-editor-line) &key line index)
  (add-child self
             (setf (.index-label self)
                   (make-instance 'label :value (format nil "~2,'0X" index)
                                         :x 0 :y 0
                                         :width (* *char-width* 2)
                                         :height *char-height*
                                         :color *note-color*)))
  (setf (.note-labels self)
        (loop for column across (.columns line)
              for x from 0
              collect (make-instance 'label :value "---"
                                            :x x :y 0
                                            :width (* *char-width* 3)
                                            :height *char-height*
                                            :color *note-color*)))
  (setf (.velocity-labels self)
        (loop for column across (.columns line)
              for x from 0
              collect (make-instance 'label :value "--"
                                            :x x :y 0
                                            :width (* *char-width* 2)
                                            :height *char-height*
                                            :color *velocity-color*)))
  (setf (.delay-labels self)
        (loop for column across (.columns line)
              for x from 0
              collect (make-instance 'label :value "--"
                                            :x x :y 0
                                            :width (* *char-width* 2)
                                            :height *char-height*
                                            :color *delay-color*))))

(defmethod render :before ((self pattern-editor-line) renderer)
  (let* ((editor (.parent self))
         (position (position self (.lines editor))))
    (loop for child in (append (.note-labels self)
                               (.velocity-labels self)
                               (.delay-labels self))
          do (remove-child self child))
    (loop with line = (.line self)
          with x = 3
          repeat (.length line)
          for column across (.columns line)
          for note = (.note column)
          for note-label in (.note-labels self) 
          for velocity-label in (.velocity-labels self) 
          for delay-label in (.delay-labels self) 
          do (add-child self note-label)
             (setf (.value note-label)
                   (cond ((= note off) "OFF")
                         ((= note none) "---")
                         (t (let* ((c-s-o (format nil "~a" (midino-to-note note)))
                                   (c (char c-s-o 0))
                                   (s (if (char= (char c-s-o 1) #\#)
                                          #\#
                                          #\-))
                                   (o (char c-s-o (if (char= s #\#) 2 1))))
                              (format nil "~c~c~c" c s o)))))
             (setf (.x note-label) (* *char-width* x))
             (incf x 4)
             (when (velocity-enable-p column)
               (add-child self velocity-label)
               (setf (.value velocity-label)
                     (if (<= c0 note)
                         (format nil "~2,'0X" (.velocity column))
                         "--"))
               (setf (.x velocity-label) (* *char-width* x))
               (incf x 3))
             (when (delay-enable-p column)
               (add-child self delay-label)
               (setf (.value delay-label)
                     (if (or (<= c0 note)
                             (= off note))
                         (format nil "~2,'0X" (.delay column))
                         "--"))
               (setf (.x delay-label) (* *char-width* x))
               (incf x 3)))
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
