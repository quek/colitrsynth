(in-package :colitrsynth)

(defmethod initialize-instance :after ((self automation-module) &key)
  (unless (slot-boundp self 'lines)
    (setf (.lines self)
          (make-array (.nlines self)
                      :initial-element 0.0
                      :element-type 'single-float)))
  (let ((editor (make-instance 'automation-editor
                               :model self)))
    (setf (.editor self) editor)
    (add-child self editor))
  (resized self))

(defmethod process ((self automation-module) (connection null) left right))

(defmethod render :before ((self automation-module) renderer)
  (update-labels (.editor self)))

(defmethod render ((self automation-module) renderer)
  (call-next-method))

(defmethod resized ((self automation-module))
  (let ((editor (.editor self)))
   (setf (.x editor) *layout-space*
         (.y editor) (+ *font-size* (* *layout-space* 2))
         (.width editor) (- (.width self) 10)
         (.height editor) (- (.height self) (+ 15 *font-size*)))))

