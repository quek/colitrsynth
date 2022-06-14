(in-package :colitrsynth)

(defmethod osc-frame-value ((self saw-osc))
  (* 0.5
     (- (* (mod (/ (incf (.phase self) (midino-to-freq (.note self)))
                   *sample-rate*)
                1.0)
           2.0)
        1.0)))

