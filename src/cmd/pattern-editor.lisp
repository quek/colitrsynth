(in-package :colitrsynth)

(defcmd cmd::cursor-down (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-j+))
  (when (< (max-cursor-y self) (incf (.cursor-y self)))
    (setf (.cursor-y self) 0)))

(defcmd cmd::cursor-left (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-h+))
  (let ((value (- (.cursor-x self)
                  (case (mod (.cursor-x self) +column-width+)
                    (0 2)
                    (4 4)
                    (5 1)
                    (t 0)))))
    (setf (.cursor-x self)
          (if (<= 0 value)
              value
              (max-cursor-x self)))))

(defcmd cmd::cursor-right (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-l+))
  (let* ((value (+ (.cursor-x self)
                   (case (mod (.cursor-x self) +column-width+)
                     (0 4)
                     (4 1)
                     (5 2)
                     (t 0)))))
    (setf (.cursor-x self)
          (if (<= value (max-cursor-x self))
              value
              0))))

(defcmd cmd::cursor-up (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-k+))
  (when (minusp (decf (.cursor-y self)))
    (setf (.cursor-y self) (max-cursor-y self))))

(defcmd cmd::escape (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-escape+))
  (cond ((eq :insert (.mode self))
         (setf (.mode self) :command))
        ((.selection-mode self)
         (setf (.selection-mode self) nil))))

(defcmd cmd::escape-insert-mode (self)
    (:next-keymap *pattern-editor-keymap*)
  (setf (.mode self) :command))

(setf (gethash (list sdl2-ffi:+sdl-scancode-escape+ 0) *pattern-editor-insert-note-keymap*)
      'cmd::escape-insert-mode)
(setf (gethash (list sdl2-ffi:+sdl-scancode-escape+ 0) *pattern-editor-insert-velocity-keymap*)
      'cmd::escape-insert-mode)
(setf (gethash (list sdl2-ffi:+sdl-scancode-escape+ 0) *pattern-editor-insert-fx-keymap*)
      'cmd::escape-insert-mode)

(defcmd cmd::insert-mode (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-i+))
  ;; TODO mode 必要？
  (setf (.mode self) :insert)
  (setf (.keymap self)
        (cond ((at-note-column-p self)
               *pattern-editor-insert-note-keymap*)
              ((at-velocity-column-p self)
               *pattern-editor-insert-velocity-keymap*)
              (t
               *pattern-editor-insert-fx-keymap*))))

(macrolet ((m (note key)
             (let ((name (intern (format nil "INSERT-NOTE-~a" note) :cmd)))
               `(progn
                  (defcmd ,name (self)
                      (:bind (*pattern-editor-insert-note-keymap* ,key))
                    (set-note self ,(intern (format nil "~a0" note))))
                  (setf (gethash (list ,key +shift+) *pattern-editor-insert-note-keymap*)
                        ',name))))
           (m+1 (note key)
             (let ((name (intern (format nil "INSERT-NOTE-~a+1" note) :cmd)))
               `(progn
                  (defcmd ,name (self)
                      (:bind (*pattern-editor-insert-note-keymap* ,key))
                    (set-note self ,(intern (format nil "~a1" note))))
                  (setf (gethash (list ,key +shift+) *pattern-editor-insert-note-keymap*)
                        ',name))))
           (m+2 (note key)
             (let ((name (intern (format nil "INSERT-NOTE-~a+2" note) :cmd)))
               `(progn
                  (defcmd ,name (self)
                      (:bind (*pattern-editor-insert-note-keymap* ,key))
                    (set-note self ,(intern (format nil "~a2" note))))
                  (setf (gethash (list ,key +shift+) *pattern-editor-insert-note-keymap*)
                        ',name)))))
  ;; http://sdl2referencejp.osdn.jp/SDLScancodeLookup.html
  (m c sdl2-ffi:+sdl-scancode-z+)
  (m c# sdl2-ffi:+sdl-scancode-s+)
  (m d sdl2-ffi:+sdl-scancode-x+)
  (m d# sdl2-ffi:+sdl-scancode-d+)
  (m e sdl2-ffi:+sdl-scancode-c+)
  (m f sdl2-ffi:+sdl-scancode-v+)
  (m f# sdl2-ffi:+sdl-scancode-g+)
  (m g sdl2-ffi:+sdl-scancode-b+)
  (m g# sdl2-ffi:+sdl-scancode-h+)
  (m a sdl2-ffi:+sdl-scancode-n+)
  (m a# sdl2-ffi:+sdl-scancode-j+)
  (m b sdl2-ffi:+sdl-scancode-m+)
  (m+1 c sdl2-ffi:+sdl-scancode-comma+)
  (m+1 c# sdl2-ffi:+sdl-scancode-l+)
  (m+1 d sdl2-ffi:+sdl-scancode-period+)
  (m+1 d# sdl2-ffi:+sdl-scancode-semicolon+)
  (m+1 e sdl2-ffi:+sdl-scancode-slash+)
  (m+1 f sdl2-ffi:+sdl-scancode-nonusbackslash+)
  (m+1 f# sdl2-ffi:+sdl-scancode-backslash+)
  (m+1 c sdl2-ffi:+sdl-scancode-q+)
  (m+1 c# sdl2-ffi:+sdl-scancode-2+)
  (m+1 d sdl2-ffi:+sdl-scancode-w+)
  (m+1 d# sdl2-ffi:+sdl-scancode-3+)
  (m+1 e sdl2-ffi:+sdl-scancode-e+)
  (m+1 f sdl2-ffi:+sdl-scancode-r+)
  (m+1 f# sdl2-ffi:+sdl-scancode-5+)
  (m+1 g sdl2-ffi:+sdl-scancode-t+)
  (m+1 g# sdl2-ffi:+sdl-scancode-6+)
  (m+1 a sdl2-ffi:+sdl-scancode-y+)
  (m+1 a# sdl2-ffi:+sdl-scancode-7+)
  (m+1 b sdl2-ffi:+sdl-scancode-u+)
  (m+2 c sdl2-ffi:+sdl-scancode-i+)
  (m+2 c# sdl2-ffi:+sdl-scancode-9+)
  (m+2 d sdl2-ffi:+sdl-scancode-o+)
  (m+2 d# sdl2-ffi:+sdl-scancode-0+)
  (m+2 e sdl2-ffi:+sdl-scancode-p+)
  (m+2 f sdl2-ffi:+sdl-scancode-leftbracket+) ;@
  (m+2 f# sdl2-ffi:+sdl-scancode-equals+) ;^
  (m+2 g sdl2-ffi:+sdl-scancode-rightbracket+)  ;[
  (m+2 g# sdl2-ffi:+sdl-scancode-international3+) ;\
  )

(defcmd cmd::insert-velociy (self) ()
  (let ((value (case (car *current-key*)
                 (#.sdl2-ffi:+sdl-scancode-0+ 0)
                 (#.sdl2-ffi:+sdl-scancode-1+ 1)
                 (#.sdl2-ffi:+sdl-scancode-2+ 2)
                 (#.sdl2-ffi:+sdl-scancode-3+ 3)
                 (#.sdl2-ffi:+sdl-scancode-4+ 4)
                 (#.sdl2-ffi:+sdl-scancode-5+ 5)
                 (#.sdl2-ffi:+sdl-scancode-6+ 6)
                 (#.sdl2-ffi:+sdl-scancode-7+ 7)
                 (#.sdl2-ffi:+sdl-scancode-8+ 8)
                 (#.sdl2-ffi:+sdl-scancode-9+ 9)
                 (#.sdl2-ffi:+sdl-scancode-a+ #xa)
                 (#.sdl2-ffi:+sdl-scancode-b+ #xb)
                 (#.sdl2-ffi:+sdl-scancode-c+ #xc)
                 (#.sdl2-ffi:+sdl-scancode-d+ #xd)
                 (#.sdl2-ffi:+sdl-scancode-e+ #xe)
                 (#.sdl2-ffi:+sdl-scancode-f+ #xf)
                 (t nil))))
    (print (.velocity (current-column self)))
    (cond ((and (at-velocity-#x0-p self)
                (<= value 8))
           (setf (ldb (byte 4 4) (.velocity (current-column self)))
                 value))
          ((at-velocity-#0x-p self)
           (setf (ldb (byte 4 0) (.velocity (current-column self)))
                 value)))))

(loop for i in (list sdl2-ffi:+sdl-scancode-0+
                     sdl2-ffi:+sdl-scancode-1+
                     sdl2-ffi:+sdl-scancode-2+
                     sdl2-ffi:+sdl-scancode-3+
                     sdl2-ffi:+sdl-scancode-4+
                     sdl2-ffi:+sdl-scancode-5+
                     sdl2-ffi:+sdl-scancode-6+
                     sdl2-ffi:+sdl-scancode-7+
                     sdl2-ffi:+sdl-scancode-8+
                     sdl2-ffi:+sdl-scancode-9+
                     sdl2-ffi:+sdl-scancode-a+
                     sdl2-ffi:+sdl-scancode-b+
                     sdl2-ffi:+sdl-scancode-c+
                     sdl2-ffi:+sdl-scancode-d+
                     sdl2-ffi:+sdl-scancode-e+
                     sdl2-ffi:+sdl-scancode-f+)
      do (setf (gethash (list i 0) *pattern-editor-insert-velocity-keymap*)
               'cmd::insert-velociy))

(defcmd cmd::octave-down (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f1+))
  (setf (.octave self) (mod (1- (.octave self)) 10)))

(defcmd cmd::octave-up (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f2+))
  (setf (.octave self) (mod (1+ (.octave self)) 10)))

(defcmd cmd::edit-step-double (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f4+))
  (setf (.edit-step self) (max 1 (* (.edit-step self) 2))))

(defcmd cmd::edit-step-half (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f3+))
  (setf (.edit-step self) (floor (/ (.edit-step self) 2))))

(defcmd cmd::paste (self)
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-p+))
  (awhen (deserialize (sdl2-ffi.functions:sdl-get-clipboard-text))
    (cond ((typep it 'line)
           (setf (current-line self) it))
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
     (write (serialize (current-line self)) :stream out))))

(defcmd cmd::yank-selection-lines (self)
    (:bind (*pattern-editor-selection-line-keymap* sdl2-ffi:+sdl-scancode-y+)
      :next-keymap *pattern-editor-keymap*)
  (let* ((start  (.cursor-y self))
         (end (.selection-start self))
         (lines (loop for i from (min start end) to (max start end)
                      collect (aref (.lines (.pattern self)) i))))
    (sdl2-ffi.functions:sdl-set-clipboard-text
     (with-serialize-context (out)
       (write (serialize lines) :stream out))))
  (setf (.selection-mode self) nil)
  ;; TODO カーソル位置を選択開始位置に戻すべき？ vim は戻している
  )
