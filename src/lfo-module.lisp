(in-package :colitrsynth)

(defmethod initialize-instance :after ((self lfo-module) &key)
  (add-child self
             (setf (.frequency-slider self)
                   (make-instance 'slider
                                  :max 2000.0
                                  :compute-function #'compute-expt
                                  :value (lambda () (.frequency self))
                                  :onchange (lambda (x)
                                              (setf (.frequency self) x)))))
  (resized self))

(defmethod cable-buffer ((module lfo-module) connection)
  (values (.buffer module) nil))

(defmethod process-out ((self lfo))
  (let ((old-phase (.phase self)))
    (loop for i below *frames-per-buffer*
          for last-phase = (.phase self)
          for value = (let ((value (sin (incf (.phase self)
                                              (/ (* 2 pi (.frequency self)) *sample-rate*)))))
                        (if (.unipolar-p self)
                            (/ (1+ value) 2)
                            value))
          do (setf (aref (.buffer self) i) value))
    (when (= old-phase (.phase self))
      (setf (.phase self) 0.0)))
  (route self (.buffer self) (.buffer self)))

(defmethod resized ((self lfo-module))
  (let ((slider (.frequency-slider self))
        (x *layout-space*)
        (y (+ *font-size* (* *layout-space* 2)))
        (width (- (.width self) (* 2 *layout-space*)))
        (height (+ *font-size* (round (/ *layout-space* 2)))))
    (setf (.x slider) x)
    (setf (.y slider) y)
    (setf (.width slider) width)
    (setf (.height slider) height)
    (call-next-method)))

(defmethod serialize ((self lfo-module))
  `((setf (.frequency x) ,(.frequency self))
    ,@(call-next-method)))
