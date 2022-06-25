(in-package :colitrsynth)

;;;; 処理の都合上必要なこ
(defvar *pattern-scroll-lock* nil)

(defmethod current-line ((self editor-mixin))
  (aref (.lines (.pattern self)) (.cursor-y self)))

(defmethod (setf current-line) (line (self editor-mixin))
  (setf (aref (.lines (.pattern self)) (.cursor-y self)) line))

(defmethod (setf .cursor-y) :after (value (self editor-mixin))
  (setf (.offset-y self)
        (round (- (* *char-height* (+ value 0.7)) ;なんで 0.7?
                  (/ (.height self) 2)))))

(defmethod keydown ((self editor-mixin) value scancode mod-value)
  (aif (or (gethash *current-key* (.keymap self))
           (gethash *current-key* *pattern-editor-keymap*))
       (progn
         (funcall it self)
         t)
       (call-next-method)))

(defmethod max-cursor-y ((self editor-mixin))
  (1- (.nlines (.pattern self))))

(defmethod render :before ((self editor-mixin) renderer)
  (when (and (.playing *audio*)
             (not *pattern-scroll-lock*))
    (setf (.cursor-y self) (.current-line (.pattern self))))
  ;; TODO まいかい呼ぶ必要はない
  (update-labels self))

(defmethod step-next ((self editor-mixin))
  (setf (.cursor-y self) (mod (+ (.cursor-y self) (.edit-step self))
                              (length (.lines (.model self))))))

(defmethod update-labels ((self editor-mixin))
  (let* ((model (.model self))
         (nlines (.nlines model)))
    (when (/= (length (.index-labels self))
              nlines)
      (loop for child in (.index-labels self)
            do (remove-child self child))
      (let ((width (* *char-width* 5))
            (height *char-height*))
        (setf (.index-labels self)
              (loop for y below nlines
                    collect (make-instance 'editor-index-label
                                           :editor self
                                           :value (format nil "~2,'0X" y)
                                           :x 0
                                           :y (* *char-height* y)
                                           :width width
                                           :height height)))
        (mapc (lambda (x) (add-child self x)) (.index-labels self))))
    (loop for index below nlines
          for index-label in (.index-labels self)
          do (setf (.value index-label)
                   (format nil "~2,'0X" index)))))
