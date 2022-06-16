(in-package :colitrsynth)

(defcmd cmd::escape (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-escape+))
  (cond ((eq :insert (.mode self))
         (setf (.mode self) :command))
        ((.selection-mode self)
         (setf (.selection-mode self) nil))))

(defcmd cmd::paste (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-p+))
  (awhen (deserialize (sdl2-ffi.functions:sdl-get-clipboard-text))
    (cond ((typep it 'line)
           (setf (.current-line self) it))
          ((and (typep it 'cons)
                (typep (car it) 'line))
           (loop for i from (.cursor-y self) below (.length (.pattern self))
                 for line in it
                 do (setf (aref (.lines (.pattern self)) i) line))))))

(defcmd cmd::selection-mode-block (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-v+ +ctrl+)
      :next-keymap *pattern-editor-selection-block-keymap*)
  (setf (.selection-mode self) :block)
  (setf (.selection-start self) (.cursor-y self)))

(defcmd cmd::selection-mode-line (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-v+ +shift+)
     :next-keymap *pattern-editor-selection-line-keymap*)
  (setf (.selection-mode self) :line)
  (setf (.selection-start self) (.cursor-y self)))

(defcmd cmd::yank (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-y+)
      :next-keymap *pattern-editor-yank-keymap*))

(defcmd cmd::yank-line (self)
    (:bind (*pattern-editor-yank-keymap* sdl2-ffi:+sdl-scancode-y+)
      :next-keymap *pattern-editor-keymap*)
  (sdl2-ffi.functions:sdl-set-clipboard-text
   (with-serialize-context (out)
     (write (serialize (.current-line self)) :stream out))))

(defcmd cmd::yank-selection-lines (self)
    (:bind (*pattern-editor-selection-line-keymap* sdl2-ffi:+sdl-scancode-y+))
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
