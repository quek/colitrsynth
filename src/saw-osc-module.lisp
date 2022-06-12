(in-package :colitrsynth)

(defmethod osc-frame-value ((self saw-osc))
  (* 0.5d0
     (- (* (mod (/ (incf (.phase self) (midino-to-freq (.note self)))
                   *sample-rate*)
                1d0)
           2d0)
        1d0)))
