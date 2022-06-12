(in-package :colitrsynth)

(defmethod .name ((self midi-connection))
  "In MIDI")

(defmethod .name ((self audio-connection))
  (format nil "In ~d" (.dest-bus self)))

(defmethod .name ((self builtin-param-connection))
  (builtin-parameter-name (.param self)))

(defmethod .name ((self plugin-param-connection))
  (plugin-parameter-name (.param self)))
