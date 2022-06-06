(asdf:defsystem :colitrsynth
  :depends-on (:sdl2
               :sdl2-ttf
               :cl-portaudio
               :ieee-floats
               :cl-ppcre
               :cxml :cxml-stp :xpath
               :sb-concurrency
               :alexandria
               :anaphora)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ffi")
   (:file "prelude")
   (:file "util")
   (:file "midi")
   (:file "parameter")
   (:file "audio")
   (:file "model")
   (:file "view")
   (:file "main")))
