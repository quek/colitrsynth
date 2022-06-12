(in-package :colitrsynth)

(defmethod .name ((self midi-connection))
  "In MIDI")

(defmethod .name ((self audio-connection))
  (format nil "In ~d" (.dest-bus self)))

(defmethod .name ((self builtin-param-connection))
  (builtin-parameter-name (.param self)))

(defmethod .name ((self plugin-param-connection))
  (plugin-parameter-name (.param self)))

(defmethod connection-eql (a b)
  (and (eql (type-of a) (type-of b))
       (eql (.src a) (.src b))
       (eql (.dest a) (.dest b))))

(defmethod connection-eql ((a audio-connection) (b audio-connection))
  (and (call-next-method)
       (= (.src-bus a)
          (.src-bus b))
       (= (.dest-bus a)
          (.dest-bus b))))

(defmethod connection-eql ((a builtin-param-connection) (b builtin-param-connection))
  (eql (builtin-parameter-accessor (.param a))
       (builtin-parameter-accessor (.param b))))

(defmethod connection-eql ((a plugin-param-connection) (b plugin-param-connection))
  (= (plugin-parameter-index (.param a))
     (plugin-parameter-index (.param b))))

(defmethod connect (connection)
  (push connection (.out (.src connection)))
  (push connection (.in (.dest connection))))

(defmethod connected-p (connection)
  (member connection (.in (.dest connection))
          :test #'connection-eql))

(defmethod disconnect (connection)
  (setf (.out (.src connection))
        (remove connection (.out (.src connection))
                :test #'connection-eql))
  (setf (.in (.dest connection))
        (remove connection (.in (.dest connection))
                :test #'connection-eql)))

(defmethod disconnect-all ((self model))
  (loop for connection in (copy-list (.in self))
        do (disconnect connection))
  (loop for connection in (copy-list (.out self))
        do (disconnect connection)))
