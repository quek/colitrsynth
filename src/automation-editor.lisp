(in-package :colitrsynth)

(defmethod at-value-x0-p ((self automation-editor) x)
  (zerop x))

(defmethod at-value-0x-p ((self automation-editor) x)
  (= x 1))

(defmethod cursor-width ((self automation-editor))
  *char-width*)

(defmethod max-cursor-x ((self automation-editor))
  1)

(defmethod update-labels ((self automation-editor))
  (call-next-method)
  (let* ((model (.model self))
         (nlines (.nlines model)))
    (when (/= (length (.value-labels self))
              nlines)
      (loop for child in (.value-labels self)
            do (remove-child self child))
      (let ((x (* *char-width* 3))
            (width (* *char-width* 2))
            (height *char-height*))
        (setf (.value-labels self)
              (loop for y below nlines
                    collect (make-instance 'label
                                           :value "--"
                                           :x x
                                           :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (mapc (lambda (x) (add-child self x)) (.value-labels self))))
    (loop for index below nlines
          for value = (aref (.lines model) index)
          for value-label in (.value-labels self)
          do (setf (.value value-label)
                   (format nil "~2,'0X" (round (* #xff value)))))))
