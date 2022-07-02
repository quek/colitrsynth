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
               :midi
               :anaphora)
  :pathname "src"
  :serial t
  :components
  ((:file "sdl2-ttf")
   (:file "package")
   (:file "ffi")
   (:file "midi-device")
   (:file "prelude")
   (:file "parameter")
   (:file "util")
   (:file "midi")
   (:file "keymap")
   
   (:file "protocol")
   (:file "class")
   (:file "view")
   (:file "model")
   (:file "column")
   (:file "connection")
   (:file "module")
   (:file "adsr-module")
   (:file "automation-module")
   (:file "constant-module")
   (:file "gain-module")
   (:file "lfo-module")
   (:file "line")
   (:file "operand-module")
   (:file "osc-module")
   (:file "sin-osc-module")
   (:file "saw-osc-module")
   (:file "master-module")
   (:file "pattern-editor-label")
   (:file "editor-mixin")
   (:file "automation-editor")
   (:file "pattern-editor")
   (:file "pattern-module")
   (:file "plugin-module")
   (:file "effect-plugin-module")
   (:file "instrument-plugin-module")
   (:file "pattern-position")
   (:file "track")
   (:file "track-head-view")
   (:file "sequencer-module")
   (:file "app")

   (:file "import-midi")
   (:file "menu")
   (:file "command-dialog")
   (:file "audio")
   ;; (:file "tracker")
   (:file "main")

   (:module "cmd"
    :serial t
    :pathname "cmd"
    :components
    ((:file "app")
     (:file "module")
     (:file "editor-mixin")
     (:file "automation-editor")
     (:file "automation-module")
     (:file "pattern-editor")
     (:file "pattern-module")
     (:file "sequencer")))))
