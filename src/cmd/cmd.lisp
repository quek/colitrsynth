(in-package :cmd)

(defmacro defcmd (name args (&key bind next-keymap) &body body)
  `(progn
     (defmethod ,name ,args
       ,@body
       ,@(when next-keymap
           `((setf (colitrsynth::.keymap self) ,next-keymap))))
     ,@(when bind
         `((setf (gethash (list ,(cadr bind) (+ ,@(cddr bind)))
                          ,(car bind))
                 ',name)))))

(import 'defcmd :colitrsynth)



