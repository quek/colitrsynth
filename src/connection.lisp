(in-package :colitrsynth)

(defmethod .name ((self midi-connection))
  "In MIDI")

(defmethod .name ((self audio-connection))
  (format nil "In ~d" (.dest-bus self)))

(defmethod .name ((self param-connection))
  (plugin-parameter-name (.param self)))
