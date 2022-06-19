(in-package :colitrsynth)

(defmethod mousebuttondown ((self app) button state clicks x y)
  (setf (.selected-module *app*) nil)
  (call-next-method))

(defmethod render ((self app) renderer)
  (let ((drag-state (.drag-state *app*)))
    (when (and drag-state
               (eq self (.target drag-state))
               (.dragging drag-state))
      (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
        (let* ((x (min mouse-x (.x drag-state)))
               (y (min mouse-y (.y drag-state)))
               (w (abs (- mouse-x (.x drag-state))))
               (h (abs (- mouse-y (.y drag-state)))))
          (sdl2:render-draw-rect renderer
                                 (sdl2:make-rect x y w h))))))
  (call-next-method))
