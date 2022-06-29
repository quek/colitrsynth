(in-package :colitrsynth)

(defcmd cmd::hide ((self module)) (:interactive t)
  (setf (show-p self) nil))

