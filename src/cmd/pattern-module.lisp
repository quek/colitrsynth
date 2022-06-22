(in-package :colitrsynth)

(defcmd cmd::t ((self pattern-module) delta) (:interactive t)
  (loop for line across (.lines self)
        do (loop for column across (.columns line)
                 if (valid-note-p (.note column))
                   do (setf (.note column)
                            (min ga (max c0 (+ (.note column) delta)))))))
