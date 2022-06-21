(in-package :colitrsynth)

(defmethod initialize-instance :after ((self command-dialog) &key)
  (add-child self  (.text self)))
