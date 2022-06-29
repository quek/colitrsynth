(in-package :colitrsynth)

(defun open-menu (class &rest args)
  (multiple-value-bind (window-width window-height)
      (sdl2:get-window-size (.win *app*))
    (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
      (let* ((width 400)
             (height 300)
             (x (round (max 0 (min (- mouse-x (/ width 2)) (- window-width width)))))
             (y (round (max 0 (min (- mouse-y (/ height 2)) (- window-height height)))))
             (module (apply #'make-instance class
                            :x x :y y
                            :width width :height height
                            args)))
        (append-view module)
        (setf (.selected-modules *app*) (list module))))))

(defgeneric available-cables-src (src)
  (:method (src)
    (list (make-instance 'audio-connection :src src :dest nil))))

(defgeneric available-connections (src dest cable-src))



(defmethod initialize-instance :after ((self connector-menu-view) &key)
  (mapc (lambda (connection)
          (let ((button (make-instance
                         'menu-button
                         :label (.name connection)
                         :onclick (lambda ()
                                    (if (connected-p connection)
                                        (disconnect connection)
                                        (connect connection))))))
            (add-child self button)
            (push button (.buttons self))))
        (.available-connections self)))

(defun open-connector-menu (available-connections)
  (open-menu 'connector-menu-view :available-connections available-connections))

(defmethod initialize-instance :after ((self module-menu-view) &key)
  (let ((button (make-instance
                 'menu-button
                 :label "Manage Plugins"
                 :onclick (lambda ()
                            (sb-ext:run-program *plugin-host-exe* nil :wait nil)
                            (close self)))))
    (add-child self button)
    (push button (.buttons self)))
  (let ((button (make-instance
                 'menu-button
                 :label "Import MIDI"
                 :onclick (lambda ()
                            (import-midi)
                            (close self)))))
    (add-child self button)
    (push button (.buttons self)))
  (loop for (name class . initargs)
          in `(("Pattern" pattern-module)
               ("Sin" sin-osc-module)
               ("Saw" saw-osc-module)
               ("Adsr" adsr-module)
               ("Automation" automation-module)
               ("Lfo" lfo-module)
               ("Op Add" op-add-module)
               ("Op Multi" op-multi-module)
               ("Gain" gain-module)
               ("Constant" constant-module))
        do (let ((button (make-instance 'menu-builtin-button
                                        :label name
                                        :class class
                                        :initargs initargs)))
             (add-child self button)
             (push button (.buttons self))))
  (loop for plugin-description in (load-known-plugins)
        do (let ((button (make-instance 'menu-plugin-button
                                        :label (.name plugin-description)
                                        :plugin-description plugin-description)))
             (add-child self button)
             (push button (.buttons self)))))

(defun open-output-menu (cables)
  (open-menu 'connector-output-view :cables cables))

(defmethod initialize-instance :after ((self connector-output-view) &key)
  (dolist (cable (.cables self))
    (let ((button (make-instance
                   'menu-button
                   :label (.name-as-src cable)
                   :onclick (lambda ()
                              (setf (.cable-src *app*) cable)))))
      (add-child self button)
      (push button (.buttons self)))))


;; text 幅が renderer がないとわからないためのハック
(defmethod render :after ((self menu-view) renderer)
  (when (null (.filter self))
    (setf (.filter self) "")))

(defmethod (setf .filter) :around (value (self menu-view))
  (when (not (equal (.filter self) value))
    (call-next-method)
    (let ((regex (ppcre:create-scanner
                  (format nil ".*~{~c~^.*~}" (coerce value 'list))
                  :case-insensitive-mode t)))
      (loop for button in (.buttons self)
            with i = 0
            with x = *layout-space*
            with y = *layout-space*
            if (ppcre:scan regex (.label button))
              do (when (< (.width self) (+ x (.width button) *layout-space*))
                   (setf x *layout-space*)
                   (incf y (+ (.height button) *layout-space*)))
                 (setf (.x button) x)
                 (setf (.y button) y)
                 (incf x (+ (.width button) *layout-space*))
                 (if (< (.height self) (+ y (.height button)))
                     (remove-child self button)    
                     (add-child self button))
            else
              do (remove-child self button))
      (setf (.children self)
            (sort (.children self)
                  #'string<
                  :key (lambda (x)
                         (if (typep x 'button)
                             (string-downcase (.label x))
                             "")))))))

(defmethod keydown ((self menu-view) value scancode mod-value)
  (cond ((sdl2:scancode= scancode :scancode-escape)
         (close self))
        ((sdl2:scancode= scancode :scancode-return)
         (awhen (car (.children self))
           (click it sdl2-ffi:+sdl-button-left+ 0 0)))
        ((= value #x08)
         (setf (.filter self)
               (subseq (.filter self) 0 (max 0 (1- (length (.filter self)))))))
        ((<= value 127)
         (setf (.filter self) (format nil "~a~a" (.filter self) (code-char value))))))

(defmethod close ((self menu-view) &key abort)
  (declare (ignore abort))
  (setf (.selected-modules *app*) (delete self (.selected-modules *app*)))
  (remove-view self)
  (call-next-method))



(defmethod click ((self menu-button) button x y)
  (funcall (.onclick self))
  (close (.root-parent self)))



(defmethod click ((self menu-builtin-button) (button (eql 1)) x y)
  (let ((module (apply #'make-instance (.class self)
                    :name (.label self)
                    :x (- (.mouse-x *app*) 10)
                    :y (- (.mouse-y *app*) 10)
                    (.initargs self))))
    (append-view module)
    (setf (.selected-modules *app*) (list module)))
  (close (.root-parent self)))



(defmethod click ((self menu-plugin-button) (button (eql 1)) x y)
  (let* ((plugin-description (.plugin-description self))
         (class (if (.is-instrument plugin-description)
                    'instrument-plugin-module
                    'effect-plugin-module))
         (module (make-instance class
                                :name (.name plugin-description)
                                :x (- (.mouse-x *app*) 10)
                                :y (- (.mouse-y *app*) 10)
                                :plugin-description plugin-description)))
    (append-view module)
    (setf (.selected-modules *app*) (list module))
    (close (.root-parent self))))

(defun open-module-menu ()
  (open-menu 'module-menu-view))

(defmethod initialize-instance :after ((self find-module-menu-view) &key)
  (mapc (lambda (module)
          (let ((button (make-instance
                         'menu-button
                         :label (if (typep module 'sequencer-module)
                                    "Sequencer"
                                    (.name module))
                         :onclick (lambda ()
                                    (setf (show-p module) t)
                                    (move-to-front module)
                                    (setf (.selected-modules *app*) (list module))))))
            (add-child self button)
            (push button (.buttons self))))
        (.modules *app*)))

(defun open-find-module-menu ()
  (open-menu 'find-module-menu-view))
