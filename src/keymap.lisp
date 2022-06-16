(in-package :colitrsynth)

(defconstant +ctrl+  #b0001)
(defconstant +alt+   #b0010)
(defconstant +shift+ #b0100)

(defvar *pattern-editor-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-yank-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-selection-block-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-selection-line-keymap* (make-hash-table :test #'equal))

(defmacro defcmd (name args (&key bind next-keymap) &body body)
  `(progn
     (defmethod ,name ,args
       ,@body
       ,@(when next-keymap
           `((setf (.keymap self) ,next-keymap))))
     ,@(when bind
         `((setf (gethash (list ,(cadr bind) (+ ,@(cddr bind)))
                          ,(car bind))
                 ',name)))))
