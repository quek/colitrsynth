(in-package :colitrsynth)

(defmethod render :before ((self pattern-editor-index-label) renderer)
  (let* ((pattern-editor (.pattern-editor self))
         (pattern (.pattern pattern-editor)))

    (when (eq self (car (.index-labels pattern-editor)))
      ;; 背景色
      (when (eq (.mode pattern-editor) :insert)
        (apply #'sdl2:set-render-draw-color renderer *insert-mode-color*)
        (sdl2:render-fill-rect
         renderer
         (sdl2:make-rect (.render-x self)
                         (.render-y self)
                         (.width pattern-editor)
                         (* *char-height* (max-cursor-y pattern-editor)))))
      ;; play position
      (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
      (let ((play-x (.render-x self))
            (play-y (+ (.render-y self) (* *char-height* (.current-line pattern))))
            (play-w (.width pattern-editor))
            (play-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect play-x play-y play-w play-h)))
      ;; selection
      (case (.selection-mode pattern-editor)
        (:line
         (apply #'sdl2:set-render-draw-color renderer *selection-color*)
         (let ((cursor-x (* *char-width* 3))
               (cursor-y (+ (.render-y self) (* *char-height*
                                                (min (.cursor-y pattern-editor)
                                                     (.selection-start pattern-editor)))))
               (cursor-w (* *char-width* (1+ (max-cursor-x pattern-editor))))
               (cursor-h (* *char-height* (1+ (abs (- (.cursor-y pattern-editor)
                                                      (.selection-start pattern-editor)))))))
           (sdl2:render-fill-rect
            renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h)))))
      ;; cursor
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (let ((cursor-x (+ -1 (* *char-width* (+ (.cursor-x pattern-editor) 3))))
            (cursor-y (+ (.render-y self) (* *char-height* (.cursor-y pattern-editor))))
            (cursor-w (if (at-note-column-p pattern-editor (.cursor-x pattern-editor))
                          (* *char-width* 3)
                          *char-width*))
            (cursor-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))))))
