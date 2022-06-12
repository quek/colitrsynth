(in-package :colitrsynth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 波線
(defparameter *x* 0)

(let* ((draw-at-x 20)
       (draw-at-y 150)
       (points (loop for i to 200
                     for x =(+ draw-at-x i)
                     for y = (+ draw-at-y
                                (round (* (sin (* (/ (* 2 pi) 20.2)
                                                  (incf *x* 1)))
                                          5)))
                     collect (sdl2:make-point x y))))
  (multiple-value-bind (points num) (apply #'sdl2:points* points)
    (sdl2:render-draw-points renderer points num)))
