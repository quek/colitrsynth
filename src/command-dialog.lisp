(in-package :colitrsynth)

(defmethod initialize-instance :after ((self command-dialog) &key)
  (setf (.reader self) (lambda ()
                         (.command self)))
  (setf (.writer self) (lambda (value)
                         (setf (.command self) value)))
  (setf (.focused-view *app*) self))

(defmethod close ((self command-dialog) &key abort)
  (declare (ignore abort))
  (setf (.selected-modules *app*) (.targets self))
  (remove-view self)
  (call-next-method))

(defmethod lost-focuse ((self command-dialog))
  (call-next-method)
  (close self)
  (when (string/= "" (.command self))
    (let* ((xs (ppcre:split " +" (.command self)))
           (args (mapcar #'read-from-string (cdr xs)))
           (re (ppcre:create-scanner
                (format nil "~{~c~^.*~}" (coerce (car xs) 'list))
                :case-insensitive-mode t))
           (symbols (loop for symbol being the symbol in (find-package :cmd)
                          if (and
                              (fboundp symbol)
                              (get symbol :interactive)
                              (ppcre:scan re (symbol-name symbol)))
                            collect symbol))
           (symbol (car (sort symbols #'< :key (lambda (x)
                                                 (length (symbol-name x)))))))
      (when symbol
        (loop for x in (.targets self)
              do (ignore-errors (apply symbol x args)))))))

