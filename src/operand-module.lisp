(in-package :colitrsynth)

(defmethod cable-buffer ((module operand) connection)
  (values (.left module)
          (.right module)))

(defmethod process-in ((self operand) (conection audio-connection) left right)
  (macrolet ((m (left-lhs right-lhs)
               `(loop for i below *frames-per-buffer*
                     do (setf (aref (.left self) i)
                              (operate self
                                       ,left-lhs
                                       (aref left i)))
                        (setf (aref (.right self) i)
                              (operate self
                                       ,right-lhs
                                       (aref right i))))))

    (if (zerop (.in-count self))
        (m (.initial-value self)
           (.initial-value self))
        (m (aref (.left self) i)
           (aref (.right self) i)))))

(defmethod process-out ((self operand))
  (route self (.left self) (.right self)))

(defmethod operate ((self op-add) x y)
  (+ x y))

(defmethod operate ((self op-multi) x y)
  (* x y))
