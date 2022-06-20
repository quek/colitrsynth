(in-package :colitrsynth)

(defmethod serialize ((self line))
  `(make-instance 'line
                  :columns ,(serialize (.columns self))))
