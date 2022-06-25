(in-package :colitrsynth)

(defcmd cmd::column-extend ((self pattern-editor))
    (:bind (*pattern-editor-keymap*
            sdl2-ffi:+sdl-scancode-right+ +alt+))
  (extend-column (.pattern self)))

(defcmd cmd::column-shrink ((self pattern-editor))
    (:bind (*pattern-editor-keymap*
            sdl2-ffi:+sdl-scancode-left+ +alt+))
  (shrink-column (.pattern self))
  (when (< (max-cursor-x self) (.cursor-x self))
    (setf (.cursor-x self) 0)
    (cmd::cursor-left self)))

(defcmd cmd::cursor-left ((self pattern-editor)) ()
  (multiple-value-bind (column x index) (current-column self)
    (declare (ignore column x))
    (let* ((pattern (.pattern self))
           (cursor-x (.cursor-x self)))
      (if (or (plusp index)
              (not (at-note-column-p self cursor-x)))
          (let ((delta (cond ((at-note-column-p self cursor-x)
                              (Let ((left-column (1- index)))
                                (if (or (velocity-enable-p pattern left-column)
                                        (delay-enable-p pattern left-column))
                                    2
                                    (column-nchars self left-column))))
                             ((at-velocity-x0-p self cursor-x)
                              4)
                             ((at-velocity-0x-p self cursor-x)
                              1)
                             ((at-delay-x0-p self cursor-x)
                              (if (velocity-enable-p pattern index) 2 4))
                             ((at-delay-0x-p self cursor-x)
                              1)
                             (t 0))))
            (decf (.cursor-x self) delta))
          (let* ((last-column (1- (.ncolumns pattern)))
                 (delata (if (or (velocity-enable-p pattern last-column)
                                 (delay-enable-p pattern last-column))
                             0
                             2)))
            (setf (.cursor-x self)
                  (- (max-cursor-x self) delata)))))))

(defcmd cmd::cursor-right ((self pattern-editor)) ()
  (let* ((x (.cursor-x self))
         (delta (cond ((at-note-column-p self x)
                       4)
                      ((at-velocity-x0-p self x)
                       1)
                      ((at-velocity-0x-p self x)
                       2)
                      ((at-delay-x0-p self x)
                       1)
                      ((at-delay-0x-p self x)
                       2)
                      (t 0)))
         (value (+ x delta)))
    (setf (.cursor-x self)
          (if (<= value (max-cursor-x self))
              value
              0))))

(defcmd cmd::delay-toggle ((self pattern-editor))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-d+ +alt+))
  (multiple-value-bind (column x index) (current-column self)
    (declare (ignore column x))
    (setf (delay-enable-p (.pattern self) index)
          (not (delay-enable-p (.pattern self) index)))))


(defcmd cmd::delete ((self pattern-editor))
    (:bind (*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-d+)
      :next-keymap *pattern-editor-delete-keymap*))


(defcmd cmd::delete-at ((self pattern-editor))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-delete+))
  (let ((x (.cursor-x self))
        (column  (current-column self)))
    (cond ((at-note-column-p self x)
           (setf (.note column) none))
          ((at-velocity-p self x)
           (setf (.velocity column) *default-velocity*))
          ((at-delay-p self x)
           (setf (.delay column) 0)))
    (step-next self)))

(defcmd cmd::delete-line ((self pattern-editor))
    (:bind (*pattern-editor-delete-keymap* sdl2-ffi:+sdl-scancode-d+)
      :next-keymap *pattern-editor-command-keymap*)
  (cmd::yank-line self)
  (loop for column across (.columns (current-line self))
        do (setf (.note column) none)
           (setf (.velocity column) *default-velocity*)
           (setf (.delay column) 0)))

(defcmd cmd::delete-selection-lines ((self pattern-editor))
    (:bind (*pattern-editor-visual-keymap* sdl2-ffi:+sdl-scancode-d+)
      :next-keymap *pattern-editor-command-keymap*)
  (cmd::yank-selection-lines self)
  (let* ((start  (.cursor-y self))
         (end (.selection-start self)))
    (loop for i from (min start end) to (max start end)
          for line = (aref (.lines (.pattern self)) i)
          do (loop for column across (.columns line)
                   do (setf (.note column) none)
                      (setf (.velocity column) *default-velocity*)
                      (setf (.delay column) 0)))))

(defcmd cmd::insert-delay ((self pattern-editor))
    (:bind ((*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-0+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-1+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-2+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-3+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-4+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-5+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-6+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-7+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-8+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-9+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-a+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-b+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-c+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-d+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-e+)
            (*pattern-editor-insert-delay-keymap* sdl2-ffi:+sdl-scancode-f+)))
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
    (cond ((at-delay-x0-p self (.cursor-x self))
           (print (.delay (current-column self)))
           (setf (ldb (byte 4 4) (.delay (current-column self)))
                 value)
           (step-next self))
          ((at-delay-0x-p self (.cursor-x self))
           (setf (ldb (byte 4 0) (.delay (current-column self)))
                 value)
           (step-next self)))))

(defcmd cmd::insert-mode ((self pattern-editor)) ()
  (call-next-method)
  (setf (.keymap self)
        (cond ((at-note-column-p self (.cursor-x self))
               *pattern-editor-insert-note-keymap*)
              ((at-velocity-p self (.cursor-x self))
               *pattern-editor-insert-velocity-keymap*)
              (t
               *pattern-editor-insert-delay-keymap*))))

(macrolet ((m (note key)
             (let ((name (intern (format nil "INSERT-NOTE-~a" note) :cmd)))
               `(progn
                  (defcmd ,name ((self pattern-editor))
                      (:bind (*pattern-editor-insert-note-keymap* ,key))
                    (set-note self (+ ,(intern (format nil "~a0" note))
                                      (* 12 (.octave self)))))
                  (setf (gethash (list ,key +shift+) *pattern-editor-insert-note-keymap*)
                        ',name))))
           (m+1 (note key)
             (let ((name (intern (format nil "INSERT-NOTE-~a+1" note) :cmd)))
               `(progn
                  (defcmd ,name ((self pattern-editor))
                      (:bind (*pattern-editor-insert-note-keymap* ,key))
                    (set-note self (+ ,(intern (format nil "~a1" note))
                                      (* 12 (.octave self)))))
                  (setf (gethash (list ,key +shift+) *pattern-editor-insert-note-keymap*)
                        ',name))))
           (m+2 (note key)
             (let ((name (intern (format nil "INSERT-NOTE-~a+2" note) :cmd)))
               `(progn
                  (defcmd ,name ((self pattern-editor))
                      (:bind (*pattern-editor-insert-note-keymap* ,key))
                    (set-note self (+ ,(intern (format nil "~a2" note))
                                      (* 12 (.octave self)))))
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

(defcmd cmd::insert-note-off ((self pattern-editor))
    (:bind (*pattern-editor-insert-note-keymap* sdl2-ffi:+sdl-scancode-a+))
  (set-note self off))

(defcmd cmd::insert-velociy ((self pattern-editor))
    (:bind ((*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-0+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-1+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-2+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-3+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-4+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-5+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-6+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-7+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-8+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-9+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-a+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-b+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-c+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-d+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-e+)
            (*pattern-editor-insert-velocity-keymap* sdl2-ffi:+sdl-scancode-f+)))
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
    (cond ((and (at-velocity-x0-p self (.cursor-x self))
                (<= value 8))
           (setf (ldb (byte 4 4) (.velocity (current-column self)))
                 value)
           (step-next self))
          ((at-velocity-0x-p self (.cursor-x self))
           (setf (ldb (byte 4 0) (.velocity (current-column self)))
                 value)
           (step-next self)))))

(defcmd cmd::octave-down ((self pattern-editor))
    (:bind ((*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f1+)))
  (setf (.octave self) (mod (1- (.octave self)) 10)))

(defcmd cmd::octave-up ((self pattern-editor))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f2+))
  (setf (.octave self) (mod (1+ (.octave self)) 10)))

(defcmd cmd::edit-step-double ((self pattern-editor))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f4+))
  (setf (.edit-step self) (max 1 (* (.edit-step self) 2))))

(defcmd cmd::edit-step-half ((self pattern-editor))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-f3+))
  (setf (.edit-step self) (floor (/ (.edit-step self) 2))))

(defcmd cmd::paste ((self pattern-editor))
    (:bind (*pattern-editor-command-keymap* sdl2-ffi:+sdl-scancode-p+))
  (awhen (deserialize (sdl2-ffi.functions:sdl-get-clipboard-text))
    (cond ((typep it 'line)
           (setf (current-line self) it))
          ((and (typep it 'cons)
                (typep (car it) 'line))
           (loop for i from (.cursor-y self) below (.nlines (.pattern self))
                 for line in it
                 do (setf (aref (.lines (.pattern self)) i) line))))))

(defcmd cmd::line-extend ((self pattern-editor))
    (:bind (*pattern-editor-keymap*
            sdl2-ffi:+sdl-scancode-down+ +alt+))
  (extend-line (.pattern self)))

(defcmd cmd::line-shrink ((self pattern-editor))
    (:bind (*pattern-editor-keymap*
            sdl2-ffi:+sdl-scancode-up+ +alt+))
  (shrink-line (.pattern self))
  (when (< (max-cursor-y self) (.cursor-y self))
    (setf (.cursor-y self) (max-cursor-y self))))

(defcmd cmd::velocity-toggle ((self pattern-editor))
    (:bind (*pattern-editor-keymap* sdl2-ffi:+sdl-scancode-v+ +alt+))
  (multiple-value-bind (column x index) (current-column self)
    (declare (ignore column x))
    (setf (velocity-enable-p (.pattern self) index)
          (not (velocity-enable-p (.pattern self) index)))))
