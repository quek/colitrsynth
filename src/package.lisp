(defpackage :colitrsynth
  (:use :cl)
  (:export #:main))


(defpackage :colitrsynth.gui
  (:use :cl)
  (:export #:main)
  (:nicknames #:g))

(defpackage :colitrsynth.audio
  (:use :cl)
  (:export
   #:*audio*
   #:.lines
   #:.playing
   #:.sequencer
   #:.stream
   #:add-pattern
   #:connect
   #:play-audio
   #:stop-audio
   #:with-audio
   )
  (:nicknames #:a))
