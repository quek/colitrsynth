(in-package :colitrsynth)

(defmethod pe-escape (self)
  (cond ((eq :insert (.mode self))
         (setf (.mode self) :command))
        ((.selection-mode self)
         (setf (.selection-mode self) nil))))

(setf (gethash (list sdl2-ffi:+sdl-scancode-escape+ nil nil) *pattern-editor-keymap*)
      'pe-selection-mode-block)

(defmethod pe-paste-line (self)
  (awhen (deserialize (sdl2-ffi.functions:sdl-get-clipboard-text))
    (setf (.current-line self) it)))

(setf (gethash (list sdl2-ffi:+sdl-scancode-p+ nil nil) *pattern-editor-keymap*)
      'pe-paste-line)

(defmethod pe-selection-mode-block (self)
  (setf (.selection-mode self) :block)
  (setf (.selection-start self) (.cursor-y self)))

(setf (gethash (list sdl2-ffi:+sdl-scancode-v+ t nil) *pattern-editor-keymap*)
      'pe-selection-mode-block)

(defmethod pe-selection-mode-line (self)
  (setf (.selection-mode self) :line)
  (setf (.selection-start self) (.cursor-y self)))

(setf (gethash (list sdl2-ffi:+sdl-scancode-v+ nil t) *pattern-editor-keymap*)
      'pe-selection-mode-line)

(defmethod pe-yank (self)
  (setf (.keymap self) *pattern-editor-yank-keymap*))

(setf (gethash (list sdl2-ffi:+sdl-scancode-y+ nil nil) *pattern-editor-keymap*)
      'pe-yank)

(defmethod pe-yank-line (self)
  (sdl2-ffi.functions:sdl-set-clipboard-text
   (with-serialize-context (out)
     (write (serialize (.current-line self)) :stream out)))
  (setf (.keymap self) *pattern-editor-keymap*)
  (setf (.selection-mode self) nil))

(setf (gethash (list sdl2-ffi:+sdl-scancode-y+ nil nil) *pattern-editor-yank-keymap*)
      'pe-yank-line)
