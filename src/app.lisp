(in-package :colitrsynth)

(defmethod mousebuttondown ((self app) button state clicks x y)
  (setf (.selected-modules *app*) nil)
  (call-next-method))

(defmethod mousemotion ((self app) x y xrel yrel state)
  (sdl2-ffi.functions:sdl-set-cursor
   (sdl2-ffi.functions:sdl-create-system-cursor
    sdl2-ffi:+sdl-system-cursor-arrow+))
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

(defmethod drag-end ((self app) mouse-x mouse-y button)
  (let* ((drag-state (.drag-state *app*))
         (x (min mouse-x (.x drag-state)))
         (y (min mouse-y (.y drag-state)))
         (w (abs (- mouse-x (.x drag-state))))
         (h (abs (- mouse-y (.y drag-state)))))
    (setf (.selected-modules self)
          (loop for module in (.modules self)
                if (and (<= x (+ (.screen-x module) (.width module)))
                        (<= (.screen-x module) (+ x w))
                        (<= y (+ (.screen-y module) (.height module)))
                        (<= (.screen-y module) (+ y h)))
                  collect module))))
