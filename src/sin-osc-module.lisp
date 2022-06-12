(in-package :colitrsynth)

(defmethod osc-frame-value ((self sin-osc))
  (sin (* (/ (* 2 pi (midino-to-freq (.note self))) *sample-rate*)
          (.phase self))))
