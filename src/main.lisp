(in-package :colitrsynth)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-mop:finalize-inheritance (find-class 'model)))
(delegate-model module)
(delegate-model track-view)

(defun main ()
  (sb-thread:make-thread 'main-loop))

(defun main-loop ()
  (let ((*app* (setf *app* (make-instance 'app :width 800 :height 600))))
    (sdl2:with-init (:everything)
      (sdl2-ttf:init)
      (let ((font "c:/Windows/Fonts/msgothic.ttc"))
        (format t "Load font ~a~%" font)
        (setf (.font *app*)
              (sdl2-ttf:open-font font *font-size*)))

      (format t "Using SDL Library Version: ~D.~D.~D~%"
              sdl2-ffi:+sdl-major-version+
              sdl2-ffi:+sdl-minor-version+
              sdl2-ffi:+sdl-patchlevel+)
      (finish-output)

      (sdl2:with-window (win :title "CoLiTrSynth" :w (.width *app*) :h (.height *app*)
                             :flags '(:resizable)
                             :x 1 :y 25)    ;デバッグするのにこの位置が楽
        (setf (.win *app*) win)
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:hide-window win)
          (sdl2:show-window win)
          (format t "Beginning main loop.~%")
          (finish-output)
          
          (lepis:with-db ((format nil "~a\\Documents\\CoLiTrSynth\\"
                                  (sb-posix:getenv "USERPROFILE"))
                          :dump-threshold-second nil
                          :dump-when-close nil)
            (with-audio
              (let ((*sequencer-module* nil)
                    (*master-module* nil)
                    (models (lepis:@ 'models)))
                (if models
                    (let ((modules (loop for model in models
                                         collect (progn
                                                   (initialize model)
                                                   (make-module model)))))
                      (setf (.views *app*) modules)
                      (setf *sequencer-module*
                            (loop for module in modules
                                    thereis (and (typep module 'sequencer-module)
                                                 module)))
                      (setf (.sequencer *audio*) (.model *sequencer-module*))
                      (setf *master-module*
                            (loop for module in modules
                                    thereis (and (typep module 'master-module)
                                                 module)))
                      (setf (.master *audio*) (.model *master-module*)))
                    (progn
                      (setf *sequencer-module*
                            (make-instance 'sequencer-module))
                      (setf (.sequencer *audio*) (.model *sequencer-module*))
                      (setf *master-module*
                            (make-instance 'master-module))
                      (setf (.master *audio*) (.model *master-module*))
                      (setf (.views *app*)
                            ;; (make-plugin-test-modules)
                            (make-builtin-test-modules)
                            )))
                (setf (.sequencer *audio*) (.model *sequencer-module*))
                (setf (.master *audio*) (.model *master-module*))
                (sdl2:with-event-loop (:method :poll)
                  (:keydown (:keysym keysym)
                            (handle-sdl2-keydown-event keysym))
                  (:keyup (:keysym keysym)
                          (handle-sdl2-keyup-event keysym))
                  (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                                (handle-sdl2-mousemotion-event x y xrel yrel state))
                  (:mousebuttondown (:button button :state state :clicks clicks :x x :y y)
                                    (handle-sdl2-mousebuttondown-event button state clicks x y))
                  (:mousebuttonup (:button button :state state :clicks clicks :x x :y y)
                                  (handle-sdl2-mousebuttonup-event button state clicks x y))
                  (:idle ()
                         (handle-sdl2-idle-event renderer))
                  (:quit ()
                         (handle-sdl2-quit-event)
                         t))))))))))

(defun handle-sdl2-keydown-event (keysym)
  (let ((value (sdl2:sym-value keysym))
        (scancode (sdl2:scancode-value keysym))
        (mod-value (sdl2:mod-value keysym)))
    #+nil
    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
            (sdl2:sym-value keysym)
            scancode
            mod-value)
    (keydown (.selected-module *app*) value scancode mod-value)))

(defun handle-sdl2-keyup-event (keysym)
  (let  ((value (sdl2:sym-value keysym))
         (scancode (sdl2:scancode-value keysym))
         (mod-value (sdl2:mod-value keysym)))
    (keyup (.selected-module *app*) value scancode mod-value)))

(defun handle-sdl2-mousemotion-event (x y xrel yrel state)
  #+nil
  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
          x xrel y yrel state)
  (setf (.mouse-x *app*) x)
  (setf (.mouse-y *app*) y)
  (let ((module (or (.target (.drag-state *app*))
                    (.drag-resize-module *app*)
                    (view-at-mouse *app*))))
    (mousemotion module
                 (- x (.absolute-x module)) (- y (.absolute-y module))
                 xrel yrel state)))

(defun handle-sdl2-mousebuttondown-event (button state clicks x y)
  #+nil
  (format t "Mouse button down button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (let ((module (view-at-mouse *app*)))
    (mousebuttondown module
                     button state clicks
                     (- x (.absolute-x module)) (- y (.absolute-y module)))))

(defun handle-sdl2-mousebuttonup-event (button state clicks x y)
  #+nil
  (format t "Mouse button up button: ~a, state: ~a, clicks: ~a, x: ~a, y: ~a~%"
          button state clicks x y)
  (awhen (or (let ((drag-state (.drag-state *app*)))
               (and drag-state
                    (.dragging drag-state)
                    (.target drag-state)))
             (and (.dragging *app*)
                  (.drag-resize-module *app*))
             (view-at-mouse *app*))
    (mousebuttonup it button state clicks
                   (- x (.absolute-x it)) (- y (.absolute-y it))))
  (setf (.drag-resize-module *app*) nil)
  (setf (.dragging *app*) nil)
  (setf (click-target-module button) nil)
  (setf (.connect-from-module *app*) nil))

(defun handle-sdl2-idle-event (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 #xff)
  (sdl2:render-clear renderer)
  (sdl2:set-render-draw-color renderer #xcc #xcc #xcc *transparency*)

  (loop for view in (.views *app*)
        do (render view renderer))
  
  (sdl2:render-present renderer)
  (when (.request-stop *audio*)
    (stop))
  (sdl2:delay #.(floor (/ 1000 60.0))))   ;ms

(defun handle-sdl2-quit-event ()
  (lepis:! 'models (loop for module in (.modules *app*)
                         collect (let ((model (.model module)))
                                   (prepare-save model)
                                   model)))
  (lepis:dump-db)
  (loop for module in (.modules *app*)
        do (close module))
  (when (.font *app*)
    (sdl2-ttf:close-font (.font *app*))
    (setf (.font *app*) nil))
  (when (= 1 (sdl2-ttf:was-init))
    (sdl2-ttf:quit)))

(defun make-plugin-test-modules ()
  (let* ((line-length #x20)
         (track1 (add-new-track *sequencer-module*))
         (plugin (make-module (make-instance
                               'instrument-plugin-model
                               :x 200 :y 250
                               :plugin-description
                               (make-instance 'plugin-description
                                              :name "Zebralette"))))
         (pattern1 (make-module
                    (make-instance
                     'pattern
                     :name "Pattern1"
                     :x 5 :y 250 :height 200
                     :length line-length
                     :lines (list-to-pattern-lines
                             (list a4 none none none e5 none a5 none
                                   a4 off e5 a4 off a4 off e5
                                   a4 none none none e5 none a5 none
                                   a4 off e5 a4 off a4 off e5))))))
    (connect track1 plugin)
    (connect plugin *master-module*)
    (add-pattern track1 pattern1 0 line-length)
    (list *sequencer-module* *master-module* pattern1 plugin)))

(defun make-builtin-test-modules ()
  (let* ((line-length #x20)
         (track1 (add-new-track *sequencer-module*))
         (pattern1 (make-module
                    (make-instance
                     'pattern
                     :name "Pattern1"
                     :x 5 :y 250 :height 200
                     :length line-length
                     :lines (list-to-pattern-lines
                             (list a4 none none none e5 none a5 none
                                   a4 off e5 a4 off a4 off e5
                                   a4 none none none e5 none a5 none
                                   a4 off e5 a4 off a4 off e5)))))
         (saw (make-module (make-instance 'saw-osc :x 150 :y 250)))
         (adsr (make-module (make-instance 'adsr :x 300 :y 250)))
         (amp (make-module (make-instance 'amp :x 200 :y 400))))
    (connect track1 saw)
    (connect track1 adsr)
    (connect saw amp)
    (connect adsr amp)
    (connect amp *master-module*)
    (add-pattern track1 pattern1 0 line-length)
    (list *sequencer-module* *master-module* pattern1 saw adsr amp)))
