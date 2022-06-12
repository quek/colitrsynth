(in-package :colitrsynth)

(defmethod initialize-instance :after ((self lfo-module) &key)
  (add-child self
             (setf (.frequency-slider self)
                   (make-instance 'slider
                                  :max 2000.0d0
                                  :compute-function #'compute-expt
                                  :value (lambda () (.frequency self))
                                  :onchange (lambda (x) (setf (.frequency self) x)))))
  (resized self))

(defmethod process((self lfo) connection left right)
  (loop for i below *frames-per-buffer*
        for value = (let ((value (sin (* (/ (* 2 pi (.frequency self)) *sample-rate*)
                                         (.phase self)))))
                      (if (.unipolar-p self)
                          (/ (1+ value) 2)
                          value))
        do (setf (aref (.buffer self) i) value)
           (incf (.phase self)))
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
