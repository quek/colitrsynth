(in-package :colitrsynth)

(defmethod process-in ((self gain-module) (connection audio-connection) left right)
  (loop for i below *frames-per-buffer*
        with volume = (.volume self)
        do (setf (aref (.left self) i) (* (aref left i) volume))
           (setf (aref (.right self) i) (* (aref right i) volume))))

(defmethod process-out ((self gain-module))
  (route self (.left self) (.right self))
  (loop for i below *frames-per-buffer*
        do (setf (aref (.left self) i) 0.0d0
                 (aref (.right self) i) 0.0d0)))

(defmethod serialize ((self gain-module))
  `((setf (.volume x) ,(.volume self))
    ,@(call-next-method)))
