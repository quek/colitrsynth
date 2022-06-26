(in-package :colitrsynth)

(defmethod available-cables-src ((src track-view))
  (cons (make-instance 'midi-connection :src src :dest nil)
        (aif (find-if  (lambda (x)
                         (typep (.pattern x) 'automation-module))
                       (.pattern-positions src))
             (cons (make-instance 'param-connection
                                  :src src
                                  :dest nil
                                  :param it) ;TODO どうしよう
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
                                                               (- end-line start) end-frame)))))))
         (automation-events
           ;; TODO LFO や Constant と同じくオーディオ信号出力しなきゃだった
           ;; 下で coerce してるのも違う
           (loop for pattern-position in (.pattern-positions track)
                 if (typep (.pattern pattern-position) 'automation-module)
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
    #+nil
    (when midi-events
      (print midi-events))
    (route track (cons midi-events
                       ;; TODO なのでこれも違う
                       (coerce automation-events '(simple-array single-float (*))))
           start-frame)))

(defun play-track-all-off (track start-frame)
  (route track (cons (list (midi-event-all-notes-off)) nil) start-frame))

(defun play-track-no-notes (track start-frame)
  (route track nil start-frame))

(defmethod route-connection ((connection midi-connection)
                             (src track)
                             dest
                             midi-events-param-events start-frame)
  (process dest
           connection
           (car midi-events-param-events)
           start-frame))

(defmethod route-connection ((connection param-connection)
                             (src track)
                             dest
                             midi-events-param-events start-frame)
  (process dest
           connection
           (cdr midi-events-param-events)
           start-frame))
