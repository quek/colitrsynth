(in-package :colitrsynth)

(defmethod double-click ((self pattern-position-view)
                         (button (eql sdl2-ffi:+sdl-button-left+))
                         x y)
  (let ((pattern (.pattern self)))
    (move-to-front pattern)
    (setf (show-p pattern) t)
    (setf (.selected-modules *app*) (list pattern))))


