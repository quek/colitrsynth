(in-package :colitrsynth)

(defmethod osc-frame-value ((self sin-osc))
  (sin (incf (.phase self)
             (/ (* 2 pi (midino-to-freq (.note self))) *sample-rate*))))
