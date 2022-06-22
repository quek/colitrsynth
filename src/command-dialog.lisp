(in-package :colitrsynth)

(defmethod initialize-instance :after ((self command-dialog) &key)
  (setf (.reader self) (lambda ()
                         (.command self)))
  (setf (.writer self) (lambda (value)
                         (setf (.command self) value)))
  (setf (.focused-view *app*) self))

;;; TODO menu-view と共通化
(defmethod close ((self command-dialog) &key abort)
  (declare (ignore abort))
  (setf (.selected-modules *app*) (delete self (.selected-modules *app*)))
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
           (symbols (print (loop for symbol being the symbol in (find-package :cmd)
                                 if (ppcre:scan re (symbol-name symbol))
                                   collect symbol)))
           (symbol (print (car (sort symbols #'< :key (lambda (x)
                                                        (length (symbol-name x))))))))
      (when (and symbol
                 (fboundp symbol)
                 (get symbol :interactive))
        (loop for x in (.targets self)
              do (handler-case (apply symbol x args)
                   (sb-pcl::no-applicable-method-error ())))))
    (setf (.selected-modules *app*) (.targets self))))

