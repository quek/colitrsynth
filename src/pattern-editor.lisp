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

;; TODO もしかすると resized に移すべき？
(defmethod render :before ((self pattern-editor) renderer)
  (when (and (.playing *audio*)
             (not *pattern-scroll-lock*))
    (setf (.cursor-y self) (.current-line (.pattern self))))
  (let ((pattern-lines (.lines (.pattern self))))
    (if (/= (length pattern-lines)
            (length (.lines self)))
        (progn
          (loop for line in (.lines self)
                do (remove-child self line))
          (setf (.lines self) nil)
          (loop for pattern-line across pattern-lines
                for y from 2 by *char-height*
                for line = (make-instance 'pattern-editor-line :line pattern-line
                                                               :x 3 :y y)
                do (push line (.lines self))
                   (add-child self line))
          (setf (.lines self) (nreverse (.lines self))))
        ;; PERFORMANCE 毎回やる必要ないよね
        (loop for pattern-line across pattern-lines
              for pattern-editor-line in (.lines self)
              do (setf (.line pattern-editor-line) pattern-line)))))

(defmethod max-cursor-x ((self pattern-editor))
  "2 引くのは cursor-x が 0 オリジンと column の最後のスペース"
  (- (nchars (current-line self)) 2))

(defmethod max-cursor-y ((self pattern-editor))
  (1- (length (.lines (.pattern self)))))

(defmethod set-note ((self pattern-editor) note)
  (when (at-note-column-p self (.cursor-x self))
    (setf (.note (current-column self))
          note)))

(defmethod set-note :after ((self pattern-editor) note)
  (if (shift-key-p)
      (let ((line (current-line))
            (max-line-length 16))
        (multiple-value-bind (column x i) (current-column self)
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

