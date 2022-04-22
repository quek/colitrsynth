(asdf:defsystem :colitrsynth
  :depends-on (:sdl2
               :cl-opengl
               :cl-patterns/supercollider)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "main")))
