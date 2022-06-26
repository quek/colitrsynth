(in-package :colitrsynth)

(defmethod available-connections (src (dest gain-module) (cable-src param-connection-mixin))
  (list (make-instance
         'builtin-param-connection
         :src src :dest dest
         :param (make-builtin-parameter :name "Gain"
                                        :accessor '.volume))))

(defmethod cable-buffer ((module gain-module) (connection audio-connection))
  (values (.left module)
          (.right module)))

(defmethod process-in ((self gain-module) (connection audio-connection) left right)
  (macrolet ((m (op)
               `(loop for i below *frames-per-buffer*
                      with volume = (.volume self)
                      do (,op (aref (.left self) i) (* (aref left i) volume))
                         (,op (aref (.right self) i) (* (aref right i) volume)))))
    (if (zerop (.in-count self))
        (m setf)
        (m incf))))

(defmethod process-out ((self gain-module))
  (route self (.left self) (.right self)))

(defmethod serialize ((self gain-module))
  `((setf (.volume x) ,(.volume self))
    ,@(call-next-method)))
