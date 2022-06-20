(in-package :colitrsynth)

(defcmd cmd::yank ((self app)) ()
  (when (.selected-modules *app*)
    (sdl2-ffi.functions:sdl-set-clipboard-text
     (with-standard-io-syntax
       (let ((*package* (find-package :colitrsynth))
             (*serialize-table* (make-hash-table))
             (*serialize-refs* nil))
         (with-output-to-string (out)
           (mapc (lambda (module)
                   (write (serialize module) :stream out)
                   (terpri out)
                   (write (ref-id module) :stream out)
                   (terpri out))
                 (.selected-modules *app*))))))))

(defcmd cmd::paste ((self app)) ()
  (let* ((*serialize-table* (make-hash-table))
         (*serialize-refs* nil)
         (modules
           (with-input-from-string (in (sdl2-ffi.functions:sdl-get-clipboard-text))
             (with-standard-io-syntax
               (let ((*package* (find-package :colitrsynth)))
                 (loop for module = (eval (read in nil nil))
                       for ref-id = (read in nil nil)
                       while module
                       collect (setf (gethash ref-id *serialize-table*)
                                     module)))))))
    (when (and (consp modules)
               (every (lambda (x) (typep x 'view)) modules))
      (loop for i in *serialize-refs*
            do (funcall i))
      (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
        (let* ((module (car modules))
               (delta-x (- mouse-x (.x module)))
               (delta-y (- mouse-y (.y module))))
          (mapc (lambda (module)
                  (incf (.x module) delta-x)
                  (incf (.y module) delta-y)
                  (resized module))
                modules)))
      (mapc #'add-view modules))))
