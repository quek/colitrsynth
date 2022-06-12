(in-package :colitrsynth)

(defmethod initialize-instance :after ((self constant-module) &key)
  (add-child self
             (setf (.value-slider self)
                   (make-instance 'slider
                                  :max 4.0d0
                                  :min -4.0d0
                                  :compute-function #'compute-linear
                                  :value (lambda () (.value self))
                                  :onchange (lambda (x) (setf (.value self) x)))))
  (resized self))

(defmethod process-out ((self constant-module))
  (let ((buffer (.buffer self)))
    (loop for i below *frames-per-buffer*
          for value = (.value self)
          do (setf (aref buffer i) value))
    (route self buffer buffer)))

(defmethod resized ((self constant-module))
  (let ((slider (.value-slider self))
        (x *layout-space*)
        (y (+ *font-size* (* *layout-space* 2)))
        (width (- (.width self) (* 2 *layout-space*)))
        (height (+ *font-size* (round (/ *layout-space* 2)))))
    (setf (.x slider) x)
    (setf (.y slider) y)
     (setf (.width slider) width)
    (setf (.height slider) height)
    (call-next-method)))

(defmethod serialize ((self constant-module))
  `((setf (.value x) ,(.value self))
    ,@(call-next-method)))
