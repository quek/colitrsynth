(in-package :colitrsynth)

;;;; 処理の都合上必要なこ
(defvar *pattern-scroll-lock* nil)

(defmethod initialize-instance :after ((self editor-mixin) &key)
  (let ((index-label
          (make-instance 'editor-index-label
                         :editor self
                         :value "00"
                         :x 0
                         :y 0)))
    (setf (.index-label self) index-label)
    (add-child self index-label)))


(defmethod current-line ((self editor-mixin))
  (aref (.lines (.model self)) (.cursor-y self)))

(defmethod (setf current-line) (line (self editor-mixin))
  (setf (aref (.lines (.model self)) (.cursor-y self)) line))

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
  (1- (.nlines (.model self))))

(defmethod render :before ((self editor-mixin) renderer)
  (when (and (.playing *audio*)
             (not *pattern-scroll-lock*))
    (setf (.cursor-y self) (.current-line (.model self))))
  ;; TODO まいかい呼ぶ必要はない
  (update-labels self))

(defmethod step-next ((self editor-mixin))
  (setf (.cursor-y self) (mod (+ (.cursor-y self) (.edit-step self))
                              (length (.lines (.model self))))))

(defmethod update-labels ((self editor-mixin))
  (let ((index-label (.index-label self))
        (nlines (.nlines (.model self))))
    (when (/= (/ (.height index-label) *char-height*)
              nlines)
      (setf (.width index-label) (* *char-width* 4))
      (setf (.height index-label) (* *char-height* nlines))
      (setf (.value index-label)
            (with-output-to-string (out)
              (loop for i below nlines
                    do (format out
                               "~2,'0X~c" i #\cr)))))))
