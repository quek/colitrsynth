(asdf:defsystem :colitrsynth
  :depends-on (:sdl2
               ;;:sdl2-ttf
               :cl-opengl
               :cl-patterns/supercollider
               :sc-vst
               :cl-portaudio)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "midi")
   (:file "audio")
   (:file "main")))
