(in-package :colitrsynth)

(defmethod render :before ((self editor-index-label) renderer)
  (let* ((editor (.editor self))
         (model (.model editor)))

    (when (eq self (car (.index-labels editor)))
      ;; 背景色
      (when (eq (.mode editor) :insert)
        (apply #'sdl2:set-render-draw-color renderer *insert-mode-color*)
        (sdl2:render-fill-rect
         renderer
         (sdl2:make-rect (.render-x self)
                         (.render-y self)
                         (.width editor)
                         (* *char-height* (1+ (max-cursor-y editor))))))
      ;; play position
      (apply #'sdl2:set-render-draw-color renderer *play-position-color*)
      (let ((play-x (.render-x self))
            (play-y (+ (.render-y self) (* *char-height* (.current-line model))))
            (play-w (.width editor))
            (play-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect play-x play-y play-w play-h)))
      ;; selection
      (case (.selection-mode editor)
        (:line
         (apply #'sdl2:set-render-draw-color renderer *selection-color*)
         (let ((cursor-x (* *char-width* 3))
               (cursor-y (+ (.render-y self) (* *char-height*
                                                (min (.cursor-y editor)
                                                     (.selection-start editor)))))
               (cursor-w (* *char-width* (1+ (max-cursor-x editor))))
               (cursor-h (* *char-height* (1+ (abs (- (.cursor-y editor)
                                                      (.selection-start editor)))))))
           (sdl2:render-fill-rect
            renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h)))))
      ;; cursor
      (apply #'sdl2:set-render-draw-color renderer *cursor-color*)
      (let ((cursor-x (+ -1 (* *char-width* (+ (.cursor-x editor) 3))))
            (cursor-y (+ (.render-y self) (* *char-height* (.cursor-y editor))))
            (cursor-w (cursor-width editor))
            (cursor-h *char-height*))
        (sdl2:render-fill-rect
         renderer (sdl2:make-rect cursor-x cursor-y cursor-w cursor-h))))))
