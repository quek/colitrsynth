(in-package :colitrsynth)

(defmethod pe-selection-mode-line (self)
  (setf (.selection-mode self) :line)
  (setf (.selection-start self) (.cursor-y self)))

(setf (gethash (list sdl2-ffi:+sdl-scancode-v+ nil t) *pattern-editor-keymap*)
      'pe-selection-mode-line)
