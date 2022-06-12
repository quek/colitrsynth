(in-package :colitrsynth)

(defmethod .name ((self midi-connection))
  "In MIDI")

(defmethod .name ((self audio-connection))
  (format nil "In ~d" (.dest-bus self)))

(defmethod .param-name ((self builtin-parameter))
  (builtin-parameter-name self))

(defmethod .param-name ((self plugin-parameter))
  (plugin-parameter-name self))

(defmethod .name ((self param-connection))
  (.param-name (.param self)))
