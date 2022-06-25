(in-package :colitrsynth)

(defmethod cursor-width ((self automation-editor))
  (* *char-width* 2))

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
