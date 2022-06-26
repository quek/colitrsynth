(in-package :colitrsynth)

(defmethod available-cables-src ((src track-view))
  (cons (make-instance 'midi-connection :src src :dest nil)
        (aif (find-if  (lambda (x)
                         (typep (.pattern x) 'automation-module))
                       (.pattern-positions src))
             (cons (make-instance 'audio-connection :src src :dest nil)
                   nil))))

(defun midi-events-at-line-frame (pattern-position start-line start-frame end-line end-frame)
  (setf (.current-line (.pattern pattern-position)) start-line)
  (events-at-line-frame (.pattern pattern-position)
                        pattern-position start-line start-frame end-line end-frame))

(defun play-track (track start-line start-frame end-line end-frame)
  (let* ((midi-events
           (loop for pattern-position in (.pattern-positions track)
                 if (typep (.pattern pattern-position) 'pattern-module)
                   nconc (with-slots (start end) pattern-position
                           (cond ((< end-line start-line) ;ループしている場合
                                  (append
                                   (if (and (<= start start-line)
                                            (< start-line end))
                                       (midi-events-at-line-frame pattern-position
                                                                  (- start-line start) start-frame
                                                                  (1+ (- start-line start)) 0))
                                   (if (and (<= start end-line)
                                            (< end-line end))
                                       (midi-events-at-line-frame pattern-position
                                                                  (- end-line start) 0
                                                                  (- end-line start)
                                                                  end-frame))))
                                 ((and (<= start end-line)
                                       (< start-line end))
                                  (let ((start-line (- start-line start))
                                        (start-frame start-frame))
                                    (when (minusp start-line)
                                      (setf start-line 0)
                                      (setf start-frame 0))
                                    (midi-events-at-line-frame pattern-position
                                                               start-line start-frame
                                                               (- end-line start) end-frame))))))))
    (let ((pattern-position
            (find-if (lambda (x) (and (typep (.pattern x) 'automation-module)
                                      (< start-line (.end x))
                                      (< (.start x) end-line)))
                     (.pattern-positions track))))
      (when pattern-position
        (setf (.current-line (.pattern pattern-position)) start-line)
        (loop for i below *frames-per-buffer*
              with value = (aref (.lines (.pattern pattern-position)) start-line)
              do (setf (aref (.buffer track) i) value))))
    #+nil
    (when midi-events
      (print midi-events))
    (route track (cons midi-events (.buffer track)) start-frame)))

(defun play-track-all-off (track start-frame)
  (route track (cons (list (midi-event-all-notes-off)) (.buffer track)) start-frame))

(defun play-track-no-notes (track start-frame)
  (route track (cons nil (.buffer track)) start-frame))

(defmethod route-connection ((connection midi-connection)
                             (src track)
                             dest
                             midi-events-param-events start-frame)
  (process dest
           connection
           (car midi-events-param-events)
           start-frame))

(defmethod route-connection ((connection audio-connection)
                             (src track)
                             dest
                             midi-events-param-events start-frame)
  (let ((buffer (cdr midi-events-param-events)))
   (process dest connection buffer buffer)))
