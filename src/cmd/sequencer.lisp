(in-package :colitrsynth)

(defcmd cmd::bpm-down ((self sequencer-module))
    (:bind (*sequencer-keymap* sdl2-ffi:+sdl-scancode-1+))
  (decf (.bpm self)))

(defcmd cmd::bpm-up ((self sequencer-module))
    (:bind (*sequencer-keymap* sdl2-ffi:+sdl-scancode-2+))
  (incf (.bpm self)))

(defcmd cmd::delete-selected-track ((self sequencer-module))
    (:bind (*sequencer-keymap* sdl2-ffi:+sdl-scancode-delete+ +ctrl+ +shift+))
  (mapc (lambda (track)
          (delete-track self track))
        (.selected-tracks self))
  (resized self))
