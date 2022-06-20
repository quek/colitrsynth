(in-package :colitrsynth)

(defmethod at-note-column-p ((self line) index)
  (multiple-value-bind (column index)
      (column-at self index)
    (at-note-column-p column index)))

(defmethod at-delay-#x0-p ((self line) index)
  (multiple-value-bind (column index)
      (column-at self index)
    (at-delay-#x0-p column index)))

(defmethod at-delay-#0x-p ((self line) index)
  (multiple-value-bind (column index)
      (column-at self index)
    (at-delay-#0x-p column index)))

(defmethod at-velocity-#x0-p ((self line) index)
  (multiple-value-bind (column index)
      (column-at self index)
    (at-velocity-#x0-p column index)))

(defmethod at-velocity-#0x-p ((self line) index)
  (multiple-value-bind (column index)
      (column-at self index)
    (at-velocity-#0x-p column index)))

(defmethod column-at ((self line) at)
  "カラム とカラム内での index と何番目のカラムか"
  (loop for n = at then (- n (nchars column))
        for column across (.columns self)
        for i from 0
        if (< n (nchars column))
          do (return-from column-at
               (values column n i))))

(defmethod nchars ((self line))
  "スペースも含んだ文字列単位の幅"
  (loop for i below (.length self)
        sum (nchars (aref (.columns self) i))))

(defmethod serialize ((self line))
  `(make-instance 'line
                  :columns ,(serialize (.columns self))
                  :length ,(.length self)))
