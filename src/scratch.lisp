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

