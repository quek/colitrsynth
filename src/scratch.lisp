(in-package :colitrsynth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 最適化も考えないと
(defun f (a)
  (declare (double-float a))
  (+ a 0.2d0))

(disassemble #'f)

(with-open-file (out "/tmp/a.txt" :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 64))
  (write-byte (ieee-floats:encode-float64 0.123d0) out))
;;⇒ 4593527504729830064

(with-open-file (out "/tmp/a.txt" :direction :input 
                                  :element-type '(unsigned-byte 64))
  (ieee-floats:decode-float64 (read-byte out)))
;;⇒ 0.123d0

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.sbcl.org/manual/index.html#Deterministic-Profiler
(sb-profile::profile colitrsynth::process colitrsynth::process-in colitrsynth::process-out
                     read-sequence write-sequence)
(colitrsynth:main)
(sb-profile:report)
#|
  seconds  |     gc     |     consed     |  calls  |  sec/call  |  name  
--------------------------------------------------------------
    22.620 |      0.172 |  9,874,082,160 |  82,133 |   0.000275 | READ-SEQUENCE
    16.359 |      0.219 |  7,629,628,592 | 103,178 |   0.000159 | COLITRSYNTH::PROCESS-OUT
     4.080 |      0.109 |  6,702,180,240 | 137,542 |   0.000030 | COLITRSYNTH::PROCESS-IN
     0.087 |      0.000 |      8,962,400 |  16,561 |   0.000005 | WRITE-SEQUENCE
     0.000 |      0.000 |     26,840,800 | 189,131 |   0.000000 | COLITRSYNTH::PROCESS
--------------------------------------------------------------
    43.146 |      0.500 | 24,241,694,192 | 528,545 |            | Total

estimated total profiling overhead: 0.63 seconds
overhead estimation parameters:
  0.0s/call, 1.1875e-6s total profiling, 4.375e-7s internal profiling
|#
(sb-profile:unprofile) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.sbcl.org/manual/index.html#Statistical-Profiler
