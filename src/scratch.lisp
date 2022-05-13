(ql:quickload :midi)
(ql:quickload :flexi-streams)

(flex:with-output-to-sequence (out)
  (let ((midi::*midi-output* out))
    (midi::write-message (make-instance 'midi:note-on-message :velocity 1 :key 68))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq pipe (sb-win32::create-named-pipe "\\\\.\\pipe\\pluin-host"
                                        sb-win32::pipe-access-duplex
                                        sb-win32::pipe-type-byte
                                        255 0 0 100 (cffi-sys::null-pointer)))
(setf io (sb-sys:make-fd-stream pipe :input t :output t :element-type 'unsigned-byte))
(loop repeat 10 do (read-byte io))
(loop for i from 1 below 11 do (write-byte i io))
(force-output io)
(close io)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((x "C:/Users.../plugin_host.dll"))
  (sb-alien:load-shared-object x)
  (unwind-protect
       (let ((host (sb-alien:alien-funcall
                    (sb-alien:extern-alien "start" (function sb-alien:system-area-pointer)))))
         (unwind-protect
              (let ((plugin
                      (sb-alien:alien-funcall
                       (sb-alien:extern-alien "load" (function sb-alien:system-area-pointer
                                                               sb-alien:system-area-pointer sb-alien:integer))
                       host 0)))
                (sb-alien:alien-funcall
                 (sb-alien:extern-alien "edit" (function sb-alien:void
                                                         sb-alien:system-area-pointer))
                 plugin))
           (when host
             (sleep 10)
             (sb-alien:alien-funcall
              (sb-alien:extern-alien "end" (function sb-alien:void sb-alien:system-area-pointer))
              host))))
    (sb-alien:unload-shared-object x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; libffi-7.dll を libffi.dll にリネームしておく
(ql:quickload :cl-autowrap/libffi)

(cffi:define-foreign-library libsdl2-ttf
  (:darwin (:or (:framework "SDL2_ttf") (:default "libSDL2_ttf")))
  (:unix (:or "libSDL2_ttf-2.0.so.0" "libSDL2_ttf"))
  (:windows "SDL2_ttf.dll"))


(cffi:use-foreign-library libsdl2-ttf)

(autowrap:c-include "/Users/ancient/Downloads/SDL_ttf.h")




(ql:quickload :colitrsynth)
(ql:quickload :sdl2-ttf)

(sdl2:with-init (:everything)
  (sdl2-ttf:init)
  (let* ((font (sdl2-ttf:open-font
                (asdf:system-relative-pathname 'sdl2-ttf-examples "examples/PROBE_10PX_OTF.otf")
                10))
         (texture-surface (sdl2-ttf:render-text-blended font
                                                        "hello world"
                                                        255
                                                        255
                                                        255
                                                        0))
         (width (/ (sdl2:surface-width texture-surface) 2.0))
         (height (/ (sdl2:surface-height texture-surface) 2.0))
         (vertex-position-array
           (create-gl-array :float (make-array 8
                                               :initial-contents `(,(- width) ,(- height)
                                                                   ,width ,(- height)
                                                                   ,(- width) ,height
                                                                   ,width ,height))))
         )
    )
  (print (sdl2-ttf:was-init))
  (sdl2-ttf:quit)
  )

(sdl2-ffi::make-ttf-font)
(sdl2-ffi::ttf-open-font)

