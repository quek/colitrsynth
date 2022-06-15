(asdf:defsystem :colitrsynth
  :licence "GPL3"
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
   (:file "parameter")
   (:file "util")
   (:file "midi")
   
   (:file "class")
   (:file "model")
   (:file "connection")
   (:file "adsr-module")
   (:file "constant-module")
   (:file "gain-module")
   (:file "lfo-module")
   (:file "operand-module")
   (:file "osc-module")
   (:file "sin-osc-module")
   (:file "saw-osc-module")
   (:file "master-module")
   (:file "pe")
   (:file "pattern-module")
   (:file "plugin-module")
   (:file "effect-plugin-module")
   (:file "instrument-plugin-module")
   (:file "sequencer-module")

   (:file "menu")
   (:file "audio")
   (:file "view")
   (:file "main")))
