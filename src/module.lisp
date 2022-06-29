(in-package :colitrsynth)

(defmethod available-connections (src dest cable-src)
  (list (make-instance 'audio-connection :src src :dest dest
                                         :src-bus (.src-bus cable-src))))

(defmethod available-connections (src dest (cable-src midi-connection))
  nil)

(defmethod available-connections (src (dest midi-input-mixin) (cable-src midi-connection))
  (list (make-instance 'midi-connection :src src :dest dest)))

(defmethod click ((self module)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (if (ctrl-key-p)
      (pushnew self (.selected-modules *app*))
      (setf (.selected-modules *app*) (list self)))
  (move-to-front self)
  (call-next-method))

(defmethod move-to-front ((self module))
  (setf (.views *app*)
        (append (delete self (.views *app*)) (list self))))

(defmethod mousebuttondown ((self module)
                            (button (eql sdl2-ffi:+sdl-button-left+))
                            state clicks x y)
  (unless (member self (.selected-modules *app*))
    (setf (.selected-modules *app*) (list self))
    (move-to-front self))
  (call-next-method))

(defmethod render :before ((self module) renderer)
 (let ((texture (sdl2:create-texture renderer :rgba8888 :target
                                     (.width self) (.height self))))
   (sdl2:set-render-target renderer texture)
   (sdl2:set-texture-blend-mode texture :blend)
   (sdl2:set-render-draw-color renderer #x00 #x00 #x00
                               (if (member self (.selected-modules *app*))
                                   #xbb #x99)) 
   (sdl2:render-fill-rect
    renderer
    (sdl2:make-rect 0 0 (.width self) (.height self)))
   (sdl2:set-render-target renderer nil)
   (let ((dest-rect (sdl2:make-rect (.render-x self)
                                    (.render-y self)
                                    (.width self)
                                    (.height self))))
     (sdl2:render-copy renderer texture :source-rect nil :dest-rect dest-rect))
   (sdl2:destroy-texture texture)))

(defmethod render :after ((self module) renderer)
  (let ((color (cond ((member self (.selected-modules *app*))
                      *selected-module-color*)
                     ((eq self (.selected-pattern *app*))
                      *selected-pattern-color*))))
    (when color
      (apply #'sdl2:set-render-draw-color renderer color)
      (sdl2:render-draw-rect
       renderer
       (sdl2:make-rect (- (.render-x self) 2)
                       (- (.render-y self) 2)
                       (+ (.width self) 4)
                       (+ (.height self) 4))) )))

