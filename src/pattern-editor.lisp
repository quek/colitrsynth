(in-package :colitrsynth)

;;;; 処理の都合上必要なこ
(defvar *pattern-scroll-lock* nil)

(defmethod at-note-column-p ((self pattern-editor) index)
  (at-note-column-p (current-line self) index))

(defmethod at-delay-column-p ((self pattern-editor) index)
  (or (at-delay-#x0-p self index)
      (at-delay-#0x-p self index)))

(defmethod at-delay-#x0-p ((self pattern-editor) index)
  (at-delay-#x0-p (current-line self) index))

(defmethod at-delay-#0x-p ((self pattern-editor) index)
  (at-delay-#0x-p (current-line self) index))

(defmethod at-velocity-column-p ((self pattern-editor) index)
  (or (at-velocity-#x0-p self index)
      (at-velocity-#0x-p self index)))

(defmethod at-velocity-#x0-p ((self pattern-editor) index)
  (at-velocity-#x0-p (current-line self) index))

(defmethod at-velocity-#0x-p ((self pattern-editor) index)
  (at-velocity-#0x-p (current-line self) index))

(defmethod current-column ((self pattern-editor))
  (column-at (current-line self) (.cursor-x self)))

(defmethod current-line ((self pattern-editor))
  (aref (.lines (.pattern self)) (.cursor-y self)))

(defmethod (setf current-line) (line (self pattern-editor))
  (setf (aref (.lines (.pattern self)) (.cursor-y self)) line))

(defmethod (setf .cursor-y) :after (value (self pattern-editor))
  (setf (.offset-y self)
        (round (- (* *char-height* (+ value 0.7)) ;なんで 0.7?
                  (/ (.height self) 2)))))

(defmethod keydown ((self pattern-editor) value scancode mod-value)
  ;; (unless (.focused self)
  ;;   (return-from keydown 'call-next-method))
  (let ((*current-key* (list scancode (to-bind-mod-value mod-value))))
    (aif (or (gethash *current-key* (.keymap self))
               (gethash *current-key* *pattern-editor-keymap*))
        (progn
          (funcall it self)
          t)
        (call-next-method))))

(defmethod keyup ((self pattern-editor) value scancode mod-value)
  (cond ((and (.shifting-p self)
              (or (sdl2:scancode= scancode :scancode-lshift)
                  (sdl2:scancode= scancode :scancode-rshift)))
         (setf (.shifting-p self) nil)
         (step-next self)
         (setf (.cursor-x self) 0))
        (t (call-next-method))))

(defmethod render :before ((self pattern-editor) renderer)
  (when (and (.playing *audio*)
             (not *pattern-scroll-lock*))
    (setf (.cursor-y self) (.current-line (.pattern self))))
  ;; TODO まいかい呼ぶ必要はない
  (update-labels self))

(defmethod max-cursor-x ((self pattern-editor))
  "2 引くのは cursor-x が 0 オリジンと column の最後のスペース"
  (- (nchars (current-line self)) 2))

(defmethod max-cursor-y ((self pattern-editor))
  (1- (.length (.pattern self))))

(defmethod set-note ((self pattern-editor) note)
  (when (at-note-column-p self (.cursor-x self))
    (setf (.note (current-column self))
          note)))

(defmethod set-note :after ((self pattern-editor) note)
  (if (shift-key-p)
      (let ((line (current-line self))
            (max-line-length 16))
        (multiple-value-bind (column x i) (current-column self)
          (declare (ignore column x))
          (setf (.shifting-p self) t)
          (let ((new-column (1+ i)))
           (when (< new-column max-line-length)
             (when (<= (.length line) new-column)
               (extend-column (.pattern self)))
             (setf (.cursor-x self)
                   (loop for i below new-column
                         sum (nchars (aref (.columns line) i))))
             (let ((root (.root-parent self)))
               (setf (.width root) (max (.width root)
                                        (* *char-width*
                                           (+ (.cursor-x self)
                                              (nchars (aref (.columns line) new-column))
                                              3)))))))))
      (step-next self)))

(defmethod step-next ((self pattern-editor))
  (setf (.cursor-y self) (mod (+ (.cursor-y self) (.edit-step self))
                              (length (.lines (.pattern self))))))

(defmethod update-labels ((self pattern-editor))
  (let* ((pattern (.pattern self))
         (pattern-length (.length pattern)))
    (when (/= (length (.index-labels self))
              pattern-length)
      (loop for child in (append (.index-labels self)
                                 (.note-labels self)
                                 (.velocity-labels self)
                                 (.delay-labels self))
            do (remove-child self child))
      (let ((x (* *char-width* 2))
            (width (* *char-width* (max-cursor-x self)))
            (height (* *char-width* pattern-length)))
        (setf (.index-labels self)
              (loop for y below pattern-length
                    collect (make-instance 'pattern-editor-index-label
                                           :pattern-editor self
                                           :value (format nil "~2,'0X" y)
                                           :x 0 :y (* *char-height* y)
                                           :width (* *char-width* 3)
                                           :height *char-height*)))
        (setf (.note-labels self)
              (loop for y below pattern-length
                    collect (make-instance 'pattern-editor-note-label
                                           :pattern-editor self
                                           :value "-"
                                           :x x
                                           :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (setf (.velocity-labels self)
              (loop for y below pattern-length
                    collect (make-instance 'pattern-editor-velocity-label
                                           :pattern-editor self
                                           :value "-"
                                           :x x
                                           :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (setf (.delay-labels self)
              (loop for y below pattern-length
                    collect (make-instance 'pattern-editor-delay-label
                                           :pattern-editor self
                                           :value "-"
                                           :x x
                                           :y (* *char-height* y)
                                           :width width
                                           :height height))))
      (loop for child in (append (.index-labels self)
                                 (.note-labels self)
                                 (.velocity-labels self)
                                 (.delay-labels self))
            do (add-child self child)))

    (loop for index below pattern-length
          for line = (aref (.lines pattern) index)
          for index-label in (.index-labels self)
          for note-label in (.note-labels self)
          for velocity-label in (.velocity-labels self)
          for delay-label in (.delay-labels self)
          do (setf (.value index-label)
                   (format nil "~2,'0X" index))
             (setf (.value note-label)
                   (with-output-to-string (out)
                     (loop for column across (.columns line)
                           for note = (.note column)
                           repeat (.length line)
                           do (cond ((= note off) (write-string " OFF" out))
                                    ((= note none) (write-string " ---" out))
                                    (t (let* ((c-s-o (format nil "~a" (midino-to-note note)))
                                              (c (char c-s-o 0))
                                              (s (if (char= (char c-s-o 1) #\#)
                                                     #\#
                                                     #\-))
                                              (o (char c-s-o (if (char= s #\#) 2 1))))
                                         (format out " ~c~c~c" c s o))))
                              (when (velocity-enable-p column)
                                (write-string "   " out))
                              (when (delay-enable-p column)
                                (write-string "   " out)))))
             (setf (.value velocity-label)
                   (with-output-to-string (out)
                     (loop for column across (.columns line)
                           repeat (.length line)
                           do (write-string "    " out)
                              (when (velocity-enable-p column)
                                (if (<= c0 (.note column))
                                    (format out " ~2,'0X" (.velocity column))
                                    (write-string " --" out)))
                              (when (delay-enable-p column)
                                (write-string "   " out)))))
             (setf (.value delay-label)
                   (with-output-to-string (out)
                     (loop for column across (.columns line)
                           repeat (.length line)
                           do (write-string "    " out)
                              (when (velocity-enable-p column)
                                (write-string "   " out))
                              (when (delay-enable-p column)
                                (if (<= c0 (.note column))
                                    (format out " ~2,'0X" (.delay column))
                                    (write-string " --" out)))))))))

