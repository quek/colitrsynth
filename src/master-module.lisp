(in-package :colitrsynth)

(defmethod initialize-instance :after ((self master-module) &key)
  (add-child self
             (make-instance
              'label
              :x 25 :y 45
              :value (let ((f (interval-upadate-value
                               (lambda () (.last-left self))
                               0.3)))
                       (lambda ()
                         (format nil "~,5f" (funcall f))))))
  (add-child self
             (make-instance
              'label
              :x 25 :y 60
              :value (let ((f (interval-upadate-value
                               (lambda () (.last-right self))
                               0.3)))
                       (lambda ()
                         (format nil "~,5f" (funcall f)))))))

(defmethod process-in ((self master) (connection audio-connection) left right)
  (loop for i below *frames-per-buffer*
        do (incf (aref (.left self) i) (aref left i))
           (incf (aref (.right self) i) (aref right i))))

(defmethod process-out ((self master)))

(defmethod serialize ((self master-module))
  `((setf (.volume x) ,(.volume self))
    ,@(call-next-method)))
