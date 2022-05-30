(asdf:defsystem :colitrsynth
  :depends-on (:sdl2
               :sdl2-ttf
               :cl-portaudio
               :ieee-floats
               :cl-ppcre
               :cxml :cxml-stp :xpath
               :lepis
               :sb-concurrency
               :anaphora)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ffi")
   (:file "prelude")
   (:file "midi")
   (:file "audio")
   (:file "model")
   (:file "view")
   (:file "main")))
