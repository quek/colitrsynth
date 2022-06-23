(in-package :colitrsynth)

(defmethod initialize-instance :after ((self automation-module) &key)
  (unless (slot-boundp self 'lines)
    (setf (.lines self)
          (make-array (.nlines self)
                      :initial-element 0.0
                      :element-type 'single-float))))

(defmethod process ((self automation-module) (connection null) left right))

(defmethod render ((self automation-module) renderer)
  (call-next-method))
