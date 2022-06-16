(in-package :colitrsynth)

(defmethod pe-escape (self)
  (cond ((eq :insert (.mode self))
         (setf (.mode self) :command))
        ((.selection-mode self)
         (setf (.selection-mode self) nil))))

(setf (gethash (list sdl2-ffi:+sdl-scancode-escape+ nil nil) *pattern-editor-keymap*)
      'pe-selection-mode-block)

(defmethod pe-paste (self)
  (awhen (deserialize (sdl2-ffi.functions:sdl-get-clipboard-text))
    (cond ((typep it 'line)
           (setf (.current-line self) it))
          ((and (typep it 'cons)
                (typep (car it) 'line))
           (loop for i from (.cursor-y self) below (.length (.pattern self))
                 for line in it
                 do (setf (aref (.lines (.pattern self)) i) line))))))

(setf (gethash (list sdl2-ffi:+sdl-scancode-p+ nil nil) *pattern-editor-keymap*)
      'pe-paste)

(defmethod pe-selection-mode-block (self)
  (setf (.selection-mode self) :block)
  (setf (.selection-start self) (.cursor-y self))
  (setf (.keymap self) *pattern-editor-selection-block-keymap*))

(setf (gethash (list sdl2-ffi:+sdl-scancode-v+ t nil) *pattern-editor-keymap*)
      'pe-selection-mode-block)

(defmethod pe-selection-mode-line (self)
  (setf (.selection-mode self) :line)
  (setf (.selection-start self) (.cursor-y self))
  (setf (.keymap self) *pattern-editor-selection-line-keymap*))

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
  (setf (.keymap self) *pattern-editor-keymap*))

(setf (gethash (list sdl2-ffi:+sdl-scancode-y+ nil nil) *pattern-editor-yank-keymap*)
      'pe-yank-line)

(defmethod pe-yank-selection-lines (self)
  (let* ((start  (.cursor-y self))
         (end (.selection-start self))
         (lines (loop for i from (min start end) to (max start end)
                      collect (aref (.lines (.pattern self)) i))))
    (sdl2-ffi.functions:sdl-set-clipboard-text
     (with-serialize-context (out)
       (write (serialize lines) :stream out))))
  (setf (.keymap self) *pattern-editor-keymap*)
  (setf (.selection-mode self) nil)
  ;; TODO カーソル位置を選択開始位置に戻すべき？ vim は戻している
  )

(setf (gethash (list sdl2-ffi:+sdl-scancode-y+ nil nil) *pattern-editor-selection-line-keymap*)
      'pe-yank-selection-lines)
