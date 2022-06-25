(in-package :colitrsynth)

(defmethod max-cursor-y ((self editor-mixin))
  (1- (.nlines (.pattern self))))

(defmethod step-next ((self pattern-editor))
  (setf (.cursor-y self) (mod (+ (.cursor-y self) (.edit-step self))
                              (length (.lines (.model self))))))

(defmethod update-labels ((self editor-mixin))
  (let* ((model (.model self))
         (nlines (.nlines model)))
    (when (/= (length (.index-labels self))
              nlines)
      (loop for child in (.index-labels self)
            do (remove-child self child))
      (let ((width (* *char-width* 5))
            (height *char-height*))
        (setf (.index-labels self)
              (loop for y below nlines
                    collect (make-instance 'label
                                           :value (format nil "~2,'0X" y)
                                           :x 0 :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (mapc (lambda (x) (add-child self x)) (.index-labels self))))
    (loop for index below nlines
          for line = (aref (.lines model) index)
          for index-label in (.index-labels self)
          do (setf (.value index-label)
                   (format nil "~2,'0X" index
                           (round (* #xff line)))))))
