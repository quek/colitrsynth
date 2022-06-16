(in-package :colitrsynth)

(defvar *app*)
(defvar *audio* nil)
(defvar *sequencer-module*)
(defvar *master-module*)

(defconstant pi (coerce cl:pi 'single-float))

(sb-ext:defglobal *plugin-processes* nil)

(defconstant +ctrl+  #b0001)
(defconstant +alt+   #b0010)
(defconstant +shift+ #b0100)

(defvar *pattern-editor-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-yank-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-selection-block-keymap* (make-hash-table :test #'equal))
(defvar *pattern-editor-selection-line-keymap* (make-hash-table :test #'equal))
