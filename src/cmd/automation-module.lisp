(in-package :colitrsynth)

(defcmd cmd::interpolate ((self automation-module)) (:interactive t)
  (let* ((editor (.editor self))
         (start (loop for y from (.cursor-y editor) downto 0
                        thereis (and (/= (aref (.lines self) y) -1.0)
                                     y)))
         (end (loop for y from (1+ (.cursor-y editor)) below (.nlines  self)
                      thereis (and (/= (aref (.lines self) y) -1.0)
                                   y))))
    (when (and start end)
      (let* ((start-value (aref (.lines self) start))
             (end-value (aref (.lines self) end))
             (delta (/ (- end-value start-value)
                       (- end start))))
        (loop for y from (1+ start) below end
              for value = (+ start-value delta) then (+ value delta)
              do (setf (aref (.lines self) y) value))))))

