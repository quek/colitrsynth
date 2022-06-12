(in-package :colitrsynth)

(defmethod process-in ((self operand) (conection audio-connection) left right)
  (loop for i below *frames-per-buffer*
        do (setf (aref (.left self) i)
                 (operate self
                          (aref (.left self) i)
                          (aref left i)))
           (setf (aref (.right self) i)
                 (operate self
                          (aref (.right self) i)
                          (aref right i)))))

(defmethod process-out ((self operand))
  (route self (.left self) (.right self))
  (loop for i below *frames-per-buffer*
        with value = (.initial-value self)
        do (setf (aref (.left self) i) value
                 (aref (.right self) i) value)))

(defmethod operate ((self op-add) x y)
  (+ x y))

(defmethod operate ((self op-multi) x y)
  (* x y))
