(ql:quickload :sdl2)
(ql:quickload :cl-opengl)

(in-package :sdl2-examples)

(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (let ((controllers ())
              (haptic ()))

          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:gl-make-current win gl-context)
          (gl:viewport 0 0 800 600)
          (gl:matrix-mode :projection)
          (gl:ortho -2 2 -2 2 -2 2)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer)

          (format t "Opening game controllers.~%")
          (finish-output)

          (sdl2:hide-window win)
          (sdl2:show-window win)

          ;; open any game controllers
          (loop :for i :upto (- (sdl2:joystick-count) 1)
                :do (when (sdl2:game-controller-p i)
                      (format t "Found gamecontroller: ~a~%"
                              (sdl2:game-controller-name-for-index i))
                      (let* ((gc (sdl2:game-controller-open i))
                             (joy (sdl2:game-controller-get-joystick gc)))
                        (setf controllers (acons i gc controllers))
                        (when (sdl2:joystick-is-haptic-p joy)
                          (let ((h (sdl2:haptic-open-from-joystick joy)))
                            (setf haptic (acons i h haptic))
                            (sdl2:rumble-init h))))))

          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
                      (let ((scancode (sdl2:scancode-value keysym))
                            (sym (sdl2:sym-value keysym))
                            (mod-value (sdl2:mod-value keysym)))
                        (cond
                          ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                          ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                          ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
                        (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                                sym
                                scancode
                                mod-value)))

            (:keyup (:keysym keysym)
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)))

            (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                          (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                                  x xrel y yrel state))

            (:controlleraxismotion
             (:which controller-id :axis axis-id :value value)
             (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                     controller-id axis-id value))

            (:controllerbuttondown (:which controller-id)
                                   (let ((h (cdr (assoc controller-id haptic))))
                                     (when h
                                       (sdl2:rumble-play h 1.0 100))))

            (:idle ()
                   (idle win))

            (:quit () t))

          (format t "Closing opened game controllers.~%")
          (finish-output)
          ;; close any game controllers that were opened as well as any haptics
          (loop :for (i . controller) :in controllers
                :do (sdl2:game-controller-close controller)
                    (sdl2:haptic-close (cdr (assoc i haptic)))))))))

(defun idle (win)
  (gl:clear :color-buffer)
  (gl:begin :triangles)
  (gl:color 1.0 0.5 0.5)
  (gl:vertex 1.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush)
  (sdl2:gl-swap-window win)
  ;; sdl2:with-event-loop はスピンループのようで 1CPU が 100% 使用中になる
  ;; ゆっくりでいいときはスリープいれるといいかもしれない
  (sleep 0.1)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :cl-wav)

(defun sin-wave-440 ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((inch 0)
         (outch 1)
         (sample-rate 48000.0D0)
         (buffer-size 1024)
         (buffer (make-array buffer-size :element-type 'single-float)))
    (portaudio:with-audio
      (pa:with-default-audio-stream (s inch outch :sample-format :float
                                                  :sample-rate sample-rate
                                                  :frames-per-buffer buffer-size)
        (let ((ph 0.0d0))
          (loop repeat 50 do
            (loop for idx from 9 below buffer-size
                  for val = (coerce (sin ph) 'single-float)
                  do (setf (aref buffer idx) val)
                     (incf ph (* (/ 440.0 sample-rate) PI 2)))
            (portaudio:write-stream s buffer)))))))

(defun play-wav
    (&optional
       (file
        "D:/Samples/Black Lotus Audio x Dianna Artist Pack/Misc Sounds/BLA - Aah 10.wav"))
  (flet ((find-chunk (name chunks)
           (find name chunks :test #'string=
                             :key (lambda (c) (getf c :chunk-id))))
         (16bit->float (lsb msb)
           (float (if (= (logand msb #x80) #x80)
                      (- (- (/ (lognot (+ (ash (logand msb #x7f) 8) lsb))
                               (expt 2 15)))
                         1)
                      (/ (+ (ash msb 8) lsb) (expt 2 15))))))
    (let* ((wav (wav:read-wav-file file))
           (data (getf (find-chunk "data" wav) :chunk-date))
           (buf (make-array (/ (length data) 2) :element-type 'single-float)))
      (loop for i from 0 below (length data) by 4
            do (setf (aref buf (/ i 2))
                     (16bit->float (aref data (+ i 0))
                                   (aref data (+ i 1)))
                     (aref buf (1+ (/ i 2)))
                     (16bit->float (aref data (+ i 2))
                                   (aref data (+ i 3)))))
      (portaudio:with-audio
        (portaudio:with-default-audio-stream
            (s 0 2 :sample-format :float :sample-rate 44100.0D0
                   :frames-per-buffer (/ (length buf) 2))
          (loop (portaudio:write-stream s buf)))))))
