(in-package :colitrsynth)

(defcmd cmd::cursor-down ((self editor-mixin))
    (:bind ((*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-j+)
            (*pattern-editor-visual-keymap* sdl2-ffi:+sdl-scancode-j+)
            (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-down+)))
  (when (< (max-cursor-y self) (incf (.cursor-y self)))
    (setf (.cursor-y self) 0)))

(defcmd cmd::cursor-left ((self editor-mixin))
    (:bind ((*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-h+)
            (*pattern-editor-visual-keymap* sdl2-ffi:+sdl-scancode-h+)
            (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-left+))))

(defcmd cmd::cursor-right ((self editor-mixin))
    (:bind ((*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-l+)
            (*pattern-editor-visual-keymap* sdl2-ffi:+sdl-scancode-l+)
            (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-right+))))

(defcmd cmd::cursor-up ((self editor-mixin))
    (:bind ((*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-k+)
            (*pattern-editor-visual-keymap* sdl2-ffi:+sdl-scancode-k+)
            (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-up+)))
  (when (minusp (decf (.cursor-y self)))
    (setf (.cursor-y self) (max-cursor-y self))))

(defcmd cmd::edit-step-double ((self editor-mixin))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f4+))
  (setf (.edit-step self) (max 1 (* (.edit-step self) 2))))

(defcmd cmd::edit-step-half ((self editor-mixin))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f3+))
  (setf (.edit-step self) (floor (/ (.edit-step self) 2))))

(defcmd cmd::escape ((self editor-mixin))
    (:bind ((*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-escape+))
      :next-keymap *pattern-editor-command-keymap*)
  (setf (.mode self) :command)
  (setf (.selection-mode self) nil))

(defcmd cmd::insert-mode ((self editor-mixin))
    (:bind (*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-i+))
  (setf (.mode self) :insert))

(defcmd cmd::jump-0 ((self editor-mixin))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f9+))
  (setf (.cursor-y self) 0))

(defcmd cmd::jump-1/4 ((self editor-mixin))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f10+))
  (setf (.cursor-y self) (floor (* (.nlines (.model self)) 1/4))))

(defcmd cmd::jump-2/4 ((self editor-mixin))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f11+))
  (setf (.cursor-y self) (floor (* (.nlines (.model self)) 2/4))))

(defcmd cmd::jump-3/4 ((self editor-mixin))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f12+))
  (setf (.cursor-y self) (floor (* (.nlines (.model self)) 3/4))))

(defcmd cmd::selection-mode-block ((self editor-mixin))
    (:bind (*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-v+ +ctrl+)
      :next-keymap *pattern-editor-visual-keymap*)
  (setf (.selection-mode self) :block)
  (setf (.selection-start self) (.cursor-y self)))

(defcmd cmd::selection-mode-line ((self editor-mixin))
    (:bind (*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-v+ +shift+)
     :next-keymap *pattern-editor-visual-keymap*)
  (setf (.selection-mode self) :line)
  (setf (.selection-start self) (.cursor-y self)))

(defcmd cmd::yank ((self editor-mixin))
    (:bind (*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-y+)
      :next-keymap *pattern-editor-yank-keymap*))

(defcmd cmd::yank-line ((self editor-mixin))
    (:bind (*pattern-editor-yank-keymap* sdl2-ffi:+sdl-scancode-y+)
      :next-keymap *pattern-editor-command-keymap*)
  (sdl2-ffi.functions:sdl-set-clipboard-text
   (with-serialize-context (out)
     (write (serialize (current-line self)) :stream out))))

(defcmd cmd::yank-selection-lines ((self editor-mixin))
    (:bind (*pattern-editor-visual-keymap* sdl2-ffi:+sdl-scancode-y+)
      :next-keymap *pattern-editor-command-keymap*)
  (let* ((start  (.cursor-y self))
         (end (.selection-start self))
         (lines (loop for i from (min start end) to (max start end)
                      collect (aref (.lines (.model self)) i))))
    (sdl2-ffi.functions:sdl-set-clipboard-text
     (with-serialize-context (out)
       (write (serialize lines) :stream out))))
  (setf (.selection-mode self) nil))
