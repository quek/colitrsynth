(in-package :colitrsynth)

(defmethod initialize-instance :after ((self track-head-view) &key)
  (let ((solo-button (make-instance 'button :label "S"
                                            :x *layout-space*
                                            :y *layout-space*))
        (mute-button (make-instance 'button :label "M"
                                            :x (+ (* *layout-space* 2) (* *char-width* 2))
                                            :y *layout-space*)))
    (setf (.solo-button self) solo-button)
    (setf (.mute-button self) mute-button)
    (add-child self solo-button)
    (add-child self mute-button)
    
    (defmethod click ((solo-button (eql solo-button))
                      (button (eql sdl2-ffi:+sdl-button-left+))
                      x y)
      (when (setf (solo-p (.track self)) (not (solo-p (.track self))))
        (setf (mute-p (.track self)) nil))
      t)
    
    (defmethod click ((mute-button (eql mute-button))
                      (button (eql sdl2-ffi:+sdl-button-left+))
                      x y)
      (when (setf (mute-p (.track self)) (not (mute-p (.track self))))
        (setf (solo-p (.track self)) nil))
      t)
    
    (defmethod render :before ((solo-button (eql solo-button)) renderer)
      (apply #'sdl2:set-render-draw-color renderer
             (if (solo-p (.track self))
                 *solo-background-color*
                 *background-color*))
      (sdl2:render-fill-rect renderer
                             (sdl2:make-rect (.render-x solo-button)
                                             (.render-y solo-button)
                                             (.width solo-button)
                                             (.height solo-button))))

    (defmethod render :before ((mute-button (eql mute-button)) renderer)
      (apply #'sdl2:set-render-draw-color renderer
             (if (mute-p (.track self))
                 *mute-background-color*
                 *background-color*))
      (sdl2:render-fill-rect renderer
                             (sdl2:make-rect (.render-x mute-button)
                                             (.render-y mute-button)
                                             (.width mute-button)
                                             (.height mute-button))))))

(defmethod click ((self track-head-view)
                  (button (eql sdl2-ffi:+sdl-button-left+))
                  x y)
  (unless (call-next-method)
    (let ((sequencer (.parent-by-class self 'sequencer-module))
          (track (.track self)))
      (if (ctrl-key-p)
          (push track (.selected-tracks sequencer))
          (setf (.selected-tracks sequencer) (list track))))))

(defmethod drag ((self track-head-view) xrel yrel
                 (button (eql sdl2-ffi:+sdl-button-left+)))
  (incf (.y self) yrel))

(defmethod drag-end ((self track-head-view) x y
                     (button (eql sdl2-ffi:+sdl-button-left+)))
  (let* ((sequencer (.parent-by-class self 'sequencer))
         (track-heads-view (.parent self))
         (old-position (position self (.children track-heads-view)))
         (new-position (max 0 (min (round (/ (.y self) *track-height*))
                                   (1- (length (.children track-heads-view)))))))
    (setf (.children track-heads-view)
          (loop for child in (.children track-heads-view)
                for i from 0
                if (and (> old-position new-position)
                        (= i new-position))
                  collect self
                if (not (eq self child))
                  collect child
                if (and (<= old-position new-position)
                        (= i new-position))
                  collect self))
    (setf (slot-value sequencer 'tracks)
          (sort (.tracks sequencer) #'<
                :key (lambda (x)
                       (position x (.children track-heads-view)
                                 :key #'.track))))
    (resized sequencer)))

(defmethod render :before ((self track-head-view) renderer)
  (let ((sequencer (.parent-by-class self 'sequencer-module)))
    (setf (.color self)
          (if (member (.track self) (.selected-tracks sequencer))
              *selected-module-color*
              *default-color*))))

(defmethod resized ((self track-head-view))
  (let ((position (position self (.children (.parent self)))))
    (setf (.y self) (* *track-height* position)))
  (call-next-method))
