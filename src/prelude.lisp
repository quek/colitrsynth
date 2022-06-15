(in-package :colitrsynth)

(defvar *app*)
(defvar *audio* nil)
(defvar *sequencer-module*)
(defvar *master-module*)

(defconstant pi (coerce cl:pi 'single-float))

(sb-ext:defglobal *plugin-processes* nil)
