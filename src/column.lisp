(in-package :colitrsynth)

(defmethod at-note-column-p ((self column) index)
  "*C*-4 64 00"
  (<= 0 index 2))

(defmethod at-delay-#x0-p ((self column) index)
  "C-4 64 *0*0 or C-4 *0*0"
  (and (delay-enable-p self)
       (= (if (velocity-enable-p self) 7 4)
          index)))

(defmethod at-delay-#0x-p ((self column) index)
  "C-4 64 0*0* or C-4 0*0*"
  (and (delay-enable-p self)
       (= (if (velocity-enable-p self) 8 5) index)))

(defmethod at-velocity-#x0-p ((self column) index)
  "C-4 *6*4 00"
  (and (velocity-enable-p self)
       (= 4 index)))

(defmethod at-velocity-#0x-p ((self column) index)
  "C-4 6*4* 00"
  (and (velocity-enable-p self)
       (= 5 index)))

(defmethod nchars ((self column))
  "先頭のスペースも含んだ文字列単位の幅 [ C-4 64 00]"
  (+ 1
     3
     (if (velocity-enable-p self) 3 0)
     (if (delay-enable-p self) 3 0)))

(defmethod serialize ((self column))
  `(make-instance 'column
                  :note ,(.note self)
                  :velocity ,(.velocity self)
                  :velocity-enable-p ,(velocity-enable-p self)
                  :delay ,(.delay self)
                  :delay-enable-p ,(delay-enable-p self)))
