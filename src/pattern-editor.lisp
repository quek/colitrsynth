(in-package :colitrsynth)

;;;; 処理の都合上必要なこ
(defvar *pattern-scroll-lock* nil)

(defmethod at-note-column-p ((self pattern-editor) index)
  (multiple-value-bind (column x) (current-column self)
    (declare (ignore column))
    (< x 4)))

(defmethod at-delay-p ((self pattern-editor) index)
  (or (at-delay-x0-p self index)
      (at-delay-0x-p self index)))

(flet ((f (self x x1 x2)
         (multiple-value-bind (column column-x column-index) (column-at self x)
           (declare (ignore column))
           (let ((pattern (.pattern self)))
             (and (delay-enable-p pattern column-index)
                  (= (if (velocity-enable-p pattern column-index) x1 x2)
                     column-x))))))

  (defmethod at-delay-x0-p ((self pattern-editor) x)
    "C-4 64 *0*0 or C-4 *0*0"
    (f self x 7 4))

  (defmethod at-delay-0x-p ((self pattern-editor) x)
    "C-4 64 0*0* or C-4 0*0*"
    (f self x 8 5)))

 (defmethod at-velocity-p ((self pattern-editor) x)
   (or (at-velocity-x0-p self x)
       (at-velocity-0x-p self x)))

(flet ((f (self x x1)
         (multiple-value-bind (column column-x column-index) (column-at self x)
           (declare (ignore column))
           (let ((pattern (.pattern self)))
             (and (velocity-enable-p pattern column-index)
                  (= column-x x1))))))

  (defmethod at-velocity-x0-p ((self pattern-editor) x)
    (f self x 4))

  (defmethod at-velocity-0x-p ((self pattern-editor) x)
    (f self x 5)))

(defmethod column-nchars ((self pattern-editor) column-index)
  "先頭のスペースも含んだ文字列単位の幅 [ C-4 64 00]"
  (let ((pattern (.pattern self)))
   (+ 1
      3
      (if (velocity-enable-p pattern column-index) 3 0)
      (if (delay-enable-p pattern column-index) 3 0))))

(defmethod column-at ((self pattern-editor) x)
  "カラム とカラム内での x 文字目と何番目のカラムか"
  (loop with pattern = (.pattern self)
        with cursor-x = x
        for i below 16
        for column-nchars = (column-nchars self i)
        for total-nchars = column-nchars then (+ total-nchars column-nchars)
        if (< cursor-x total-nchars)
          do (return-from column-at
               (values (aref (.columns (aref (.lines pattern)
                                             (.cursor-y self)))
                             i)
                       (- cursor-x (- total-nchars column-nchars))
                       i))))

(defmethod current-column ((self pattern-editor))
  "カラム とカラム内での x 文字目と何番目のカラムか"
  (column-at self (.cursor-x self)))

(defmethod current-line ((self pattern-editor))
  (aref (.lines (.pattern self)) (.cursor-y self)))

(defmethod (setf current-line) (line (self pattern-editor))
  (setf (aref (.lines (.pattern self)) (.cursor-y self)) line))

(defmethod (setf .cursor-y) :after (value (self pattern-editor))
  (setf (.offset-y self)
        (round (- (* *char-height* (+ value 0.7)) ;なんで 0.7?
                  (/ (.height self) 2)))))

(defmethod cursor-width ((self pattern-editor))
  (if (at-note-column-p self (.cursor-x self))
      (* *char-width* 3)
      *char-width*))

(defmethod keydown ((self pattern-editor) value scancode mod-value)
  (aif (or (gethash *current-key* (.keymap self))
           (gethash *current-key* *pattern-editor-keymap*))
       (progn
         (funcall it self)
         t)
       (call-next-method)))

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
  "2 引くのは cursor-x が 0 オリジンと column の最初のスペース"
  (- (loop with pattern = (.pattern self)
           for velocity-enable-p across (.velocity-enables pattern)
           for delay-enable-p across (.delay-enables pattern)
           repeat (.ncolumns pattern)
           sum (+ 1 3
                  (if velocity-enable-p 3 0)
                  (if delay-enable-p 3 0)))
     2))

(defmethod set-note ((self pattern-editor) note)
  (when (at-note-column-p self (.cursor-x self))
    (setf (.note (current-column self))
          note)))

(defmethod set-note :after ((self pattern-editor) note)
  (if (shift-key-p)
      (let ((pattern (.pattern self))
            (max-line-length 16))
        (multiple-value-bind (column x i) (current-column self)
          (declare (ignore column x))
          (setf (.shifting-p self) t)
          (let ((new-column (1+ i)))
            (when (< new-column max-line-length)
              (when (<= (.ncolumns pattern) new-column)
                (extend-column (.pattern self)))
              (setf (.cursor-x self)
                    (loop for i below new-column
                          sum (column-nchars self i)))
              (let ((root (.root-parent self)))
                (setf (.width root) (max (.width root)
                                         (* *char-width*
                                            (+ (.cursor-x self)
                                               (column-nchars self new-column)
                                               3)))))))))
      (step-next self)))

(defmethod update-labels ((self pattern-editor))
  (call-next-method)
  (let* ((model (.model self))
         (nlines (.nlines model)))
    (when (/= (length (.note-labels self))
              nlines)
      (loop for child in (append (.note-labels self)
                                 (.velocity-labels self)
                                 (.delay-labels self))
            do (remove-child self child))
      (let ((x (* *char-width* 2))
            (width (* *char-width* (max-cursor-x self)))
            (height (* *char-width* nlines)))
        (setf (.note-labels self)
              (loop for y below nlines
                    collect (make-instance 'pattern-editor-note-label
                                           :editor self
                                           :value "-"
                                           :x x
                                           :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (setf (.velocity-labels self)
              (loop for y below nlines
                    collect (make-instance 'pattern-editor-velocity-label
                                           :editor self
                                           :value "-"
                                           :x x
                                           :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (setf (.delay-labels self)
              (loop for y below nlines
                    collect (make-instance 'pattern-editor-delay-label
                                           :editor self
                                           :value "-"
                                           :x x
                                           :y (* *char-height* y)
                                           :width width
                                           :height height))))
      (loop for child in (append (.note-labels self)
                                 (.velocity-labels self)
                                 (.delay-labels self))
            do (add-child self child)))

    (loop for index below nlines
          for line = (aref (.lines model) index)
          for note-label in (.note-labels self)
          for velocity-label in (.velocity-labels self)
          for delay-label in (.delay-labels self)
          do (setf (.value note-label)
                   (with-output-to-string (out)
                     (loop for column across (.columns line)
                           for note = (.note column)
                           for column-index below (.ncolumns model)
                           do (cond ((= note off) (write-string " OFF" out))
                                    ((= note none) (write-string " ---" out))
                                    (t (let* ((c-s-o (format nil "~a" (midino-to-note note)))
                                              (c (char c-s-o 0))
                                              (s (if (char= (char c-s-o 1) #\#)
                                                     #\#
                                                     #\-))
                                              (o (char c-s-o (if (char= s #\#) 2 1))))
                                         (format out " ~c~c~c" c s o))))
                              (when (velocity-enable-p model column-index)
                                (write-string "   " out))
                              (when (delay-enable-p model column-index)
                                (write-string "   " out)))))
             (setf (.value velocity-label)
                   (with-output-to-string (out)
                     (loop for column across (.columns line)
                           for column-index below (.ncolumns model)
                           do (write-string "    " out)
                              (when (velocity-enable-p model column-index)
                                (if (valid-note-p (.note column))
                                    (format out " ~2,'0X" (.velocity column))
                                    (write-string " --" out)))
                              (when (delay-enable-p model column-index)
                                (write-string "   " out)))))
             (setf (.value delay-label)
                   (with-output-to-string (out)
                     (loop for column across (.columns line)
                           for column-index below (.ncolumns model)
                           do (write-string "    " out)
                              (when (velocity-enable-p model column-index)
                                (write-string "   " out))
                              (when (delay-enable-p model column-index)
                                (if (or (valid-note-p (.note column))
                                        (= (.note column) off))
                                    (format out " ~2,'0X" (.delay column))
                                    (write-string " --" out)))))))))

