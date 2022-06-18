(in-package :colitrsynth)

(defconstant +ctrl+  #b0001)
(defconstant +alt+   #b0010)
(defconstant +shift+ #b0100)

(defvar *pattern-editor-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-command-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-insert-note-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-insert-velocity-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-insert-fx-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-yank-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-visual-keymap* (make-hash-table :test #'equal))

(defvar *current-key* nil)

(defmacro defcmd (name args (&key bind next-keymap) &body body)
  `(progn
     (defmethod ,name ,args
       ,@body
       ,@(when next-keymap
           `((setf (.keymap self) ,next-keymap))))
     ,@(when bind
         (loop for (keymap scancode . mod) in (if (consp (car bind))
                                                  bind
                                                  (list bind))
               collect `(setf (gethash (list ,scancode (+ ,@mod))
                                       ,keymap)
                              ',name)))))