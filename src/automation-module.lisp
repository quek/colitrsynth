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

(defmethod update-labels ((self automation-editor))
  (let* ((automation (.model self))
         (nlines (.nlines automation)))
    (when (/= (length (.index-labels self))
              nlines)
      (loop for child in (.index-labels self)
            do (remove-child self child))
      (let ((width (* *char-width* 5))
            (height *char-height*))
        (setf (.index-labels self)
              (loop for y below nlines
                    collect (make-instance 'label
                                           :value (format nil "~2,'0X --" y)
                                           :x 0 :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (mapc (lambda (x) (add-child self x)) (.index-labels self))))
    (loop for index below nlines
          for line = (aref (.lines automation) index)
          for index-label in (.index-labels self)
          do (setf (.value index-label)
                   (format nil "~2,'0X ~2,'0X" index
                           (round (* #xff line)))))))
