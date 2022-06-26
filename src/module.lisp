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
