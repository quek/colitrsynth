(in-package :colitrsynth)

(defcmd cmd::escape ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-escape+))
  (setf (.cable-src *app*) nil))

(defcmd cmd::yank ((self app)) 
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-c+ +ctrl+))
  (when (.selected-modules *app*)
    (sdl2-ffi.functions:sdl-set-clipboard-text
     ;; TODO save-song と共通化したい
     (with-standard-io-syntax
       (let ((*package* (find-package :colitrsynth))
             (*serialize-table* (make-hash-table))
             (*serialize-refs* nil))
         (with-output-to-string (out)
           (mapc (lambda (module)
                   (write (serialize module) :stream out)
                   (terpri out)
                   (write (ref-id module) :stream out)
                   (terpri out))
                 (.selected-modules *app*))))))))

(defcmd cmd::open-module-menu ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-f+))
  (open-module-menu))

(defcmd cmd::open-song ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-o+ +ctrl+))
  (sb-thread:make-thread (lambda (*app*)
                           (awhen (get-open-file-name)
                             (sb-concurrency:send-message
                              (.mbox *app*)
                              (lambda () (open-song it)))))
                         :arguments (list *app*)))

(defcmd cmd::paste ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-v+ +ctrl+))
  ;; TODO open-song と共通化したい
  (let* ((*serialize-table* (make-hash-table))
         (*serialize-refs* nil)
         (modules
           (with-input-from-string (in (sdl2-ffi.functions:sdl-get-clipboard-text))
             (with-standard-io-syntax
               (let ((*package* (find-package :colitrsynth)))
                 (loop for module = (eval (read in nil nil))
                       for ref-id = (read in nil nil)
                       while module
                       collect (setf (gethash ref-id *serialize-table*)
                                     module)))))))
    (when (and (consp modules)
               (every (lambda (x) (typep x 'view)) modules))
      (loop for i in *serialize-refs*
            do (funcall i))
      (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
        (let* ((module (car modules))
               (delta-x (- mouse-x (.x module)))
               (delta-y (- mouse-y (.y module))))
          (mapc (lambda (module)
                  (incf (.x module) delta-x)
                  (incf (.y module) delta-y)
                  (resized module))
                modules)))
      (mapc #'add-view modules))))

(defcmd cmd::play ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-space+))
  (if (.playing *audio*)
      (stop)
      (play-from-current)))

(defcmd cmd::play-from-start ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-space+ +ctrl+))
  (if (.playing *audio*)
      (stop)
      (play-from-start)))

(defcmd cmd::play-from-last ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-space+ +shift+))
  (if (.playing *audio*)
      (stop)
      (play-from-last)))

(flet ((%save-song (song-file)
         (sb-thread:make-thread
          (lambda (*app*)
            (let ((file (or song-file (get-save-file-name))))
              (when file
                (unless (alexandria:ends-with-subseq ".lisp" file)
                  (setf file (format nil "~a.lisp" file)))
                (sb-concurrency:send-message
                 (.mbox *app*)
                 (lambda () (save-song file))))))
          :arguments (list *app*))))

  (defcmd cmd::save-song ((self app))
      (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-s+ +ctrl+))
    (%save-song (.song-file *app*)))

  (defcmd cmd::save-song-as ((self app))
      (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-s+ +ctrl+ +shift+))
    (%save-song nil)))


(defcmd cmd::toggle-scroll-lock ((self app))
    (:bind (*app-keymap* sdl2-ffi:+sdl-scancode-scrolllock+))
  (setf *pattern-scroll-lock* (not *pattern-scroll-lock*)))
