(defpackage :colitrsynth.ffi
  (:use :cl)
  (:export #:get-open-file-name
           #:get-save-file-name))

(defpackage :colitrsynth
  (:use :cl :anaphora :colitrsynth.ffi)
  (:shadow #:pi)
  (:export #:main))

(defpackage :cmd
  (:use :cl :anaphora :colitrsynth.ffi :colitrsynth))
