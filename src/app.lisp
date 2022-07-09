(in-package :colitrsynth)

(defmethod drag-end ((self app) mouse-x mouse-y
                     (button (eql sdl2-ffi:+sdl-button-left+)))
  (if (.cable-src *app*)
      (multiple-value-bind (x1 y1) (sdl2:mouse-state)
        (let* ((drag-state (.drag-state *app*))
               (x2 (.x drag-state))
               (y2 (.y drag-state))
               (cable
                 (loop for module in (.modules *app*)
                       for x3 = (.screen-x module)
                       for y3 = (.screen-y module)
                         thereis (loop for cable in (.out module)
                                       for dest = (.dest cable)
                                       for x4 = (.screen-x dest)
                                       for y4 = (.screen-y dest)
                                         thereis (and (crossover-p x1 y1 x2 y2 x3 y3 x4 y4)
                                                      cable)))))
          (when cable
            (disconnect cable)
            (setf (.dest (.cable-src *app*)) (.dest cable))
            (setf (.dest cable) (.src (.cable-src *app*)))
            (connect cable)
            (connect (.cable-src *app*))
            (setf (.cable-src *app*) nil))))
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
                      collect module)))))

(defmethod drag-end ((self app) mouse-x mouse-y
                     (button (eql sdl2-ffi:+sdl-button-right+)))
  (multiple-value-bind (x1 y1) (sdl2:mouse-state)
    (let* ((drag-state (.drag-state *app*))
           (x2 (.x drag-state))
           (y2 (.y drag-state))
           (cable
             (loop for module in (.modules *app*)
                   for x3 = (.screen-center-x module)
                   for y3 = (.screen-center-y module)
                     thereis (loop for cable in (.out module)
                                   for dest = (.dest cable)
                                   for x4 = (.screen-center-x dest)
                                   for y4 = (.screen-center-y dest)
                                     thereis (and (crossover-p x1 y1 x2 y2 x3 y3 x4 y4)
                                                  cable)))))
      (when cable
        (disconnect cable)))))

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
      (if (or (.cable-src *app*)
              (= (.button drag-state) sdl2-ffi:+sdl-button-right+))
          (progn
            (apply #'sdl2:set-render-draw-color renderer *default-color*)
            (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
              (sdl2:render-draw-line renderer
                                     mouse-x mouse-y
                                     (.x drag-state) (.y drag-state))))
          (progn
            (apply #'sdl2:set-render-draw-color renderer *selected-module-color*)
            (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
              (let* ((x (min mouse-x (.x drag-state)))
                     (y (min mouse-y (.y drag-state)))
                     (w (abs (- mouse-x (.x drag-state))))
                     (h (abs (- mouse-y (.y drag-state)))))
                (sdl2:render-draw-rect renderer
                                       (sdl2:make-rect x y w h))))))))
  (call-next-method))

