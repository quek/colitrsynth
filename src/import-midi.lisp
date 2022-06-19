(in-package :colitrsynth)

(defun import-midi ()
  (sb-thread:make-thread
   (lambda (*app*)
     (awhen (get-open-file-name :filter '("MIDI" "midi" "mid"))
       (let* ((midi (midi:read-midi-file it))
              (module (%import-midi midi)))
         (sb-concurrency:send-message
          (.mbox *app*)
          (lambda ()
            (addend-view module)
            (setf (.selected-module *app*) module))))))
   :arguments (list *app*)))

(defun %import-midi (midi)
  (let* ((division (midi:midifile-division midi))
         (events
           (loop for track in (midi:midifile-tracks midi)
                 nconc (loop for x in track
                             if (or (typep x 'midi:note-on-message)
                                    (typep x 'midi:note-off-message))
                               collect x)))
         (lpb (.lpb (.sequencer *audio*)))
         (time-per-line (/ division lpb))
         (max-time (loop for event in events
                         maximize (midi:message-time event)))
         ;; 0 オリジンなので 1+
         (nlines (1+ (ceiling (/ max-time time-per-line))))
         (module (make-instance 'pattern-module
                                :length nlines
                                :x (- (.mouse-x *app*) 10)
                                :y (- (.mouse-y *app*) 10)))
         (lines (.lines module))
         (max-column 1))
    (print (list max-time division lpb nlines))
    ;; TODO トラック名を取り出してパターン名にする
    (loop for event in events
          with column = 0
          with last-midi-event = 0
          with last-note = (make-array 16 :initial-element none)
          for last-line = -1 then line
          for line = (floor (/ (midi:message-time event) time-per-line))
          for delay = (round (* (/ (mod (midi:message-time event) time-per-line)
                                   time-per-line)
                                #x100))
          do (if (typep event 'midi:note-off-message)
                 (progn
                   (if (and (= last-midi-event +midi-event-off+)
                            (= last-line line))
                       (setf max-column (max (incf column) max-column))
                       (setf column 0))
                   (when (= (aref last-note column) (midi:message-key event))
                     (setf last-midi-event +midi-event-off+)
                     (setf (aref last-note column) off)
                     (let ((column (aref (.columns (aref lines line)) column)))
                       (setf (.note column) off)
                       (setf (.velocity column) (midi:message-velocity event)))))
                 (progn
                   (if (and (= last-midi-event +midi-event-on+)
                            (= last-line line))
                       (setf max-column (max (incf column) max-column))
                       (setf column 0))
                   (setf last-midi-event +midi-event-on+)
                   (setf (aref last-note column) (midi:message-key event))
                   (let ((column (aref (.columns (aref lines line)) column)))
                     (setf (.note column) (midi:message-key event))
                     (setf (.velocity column) (midi:message-velocity event))
                     (setf (.delay column) delay)))))
    (loop for line across (.lines module)
          do (setf (.length line) (1+ max-column))
             (loop  for column across (.columns line)
                    do (setf (velocity-enable-p column) t)
                       (setf (delay-enable-p column) t)))
    (multiple-value-bind (window-width window-height)
        (sdl2:get-window-size (.win *app*))
      (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)
        (let* ((width (.width module))
               (height (.height module))
               (x (round (max 0 (min (- mouse-x (/ width 2)) (- window-width width)))))
               (y (round (max 0 (min (- mouse-y (/ height 2)) (- window-height height))))))
          (setf (.x module) x)
          (setf (.y module) y))))
    module))
