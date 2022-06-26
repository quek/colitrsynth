(in-package :colitrsynth)

(defclass tracker-partial-view (partial-view)
  ())

(defclass tracker (model module)
  ((bpm :initarg :bpm :initform 140.0 :accessor .bpm
        :type single-float)
   (lpb :initarg :lpb :initform 4 :accessor .lpb)
   (tracks :initarg :tracks :accessor .tracks :initform nil)
   (looping :initform t :accessor .looping)
   (loop-start-line :initform 0 :accessor .loop-start-line
                    :type fixnum)
   (loop-end-line :initform 0 :accessor .loop-end-line
                  :type fixnum)
   (play-position :initform (make-play-position)
                  :accessor .play-position)
   (last-play-position :initform (make-play-position)
                       :accessor .last-play-position))
  (:default-initargs :x 15 :y 15 :width 200 :height 300
                     :color (list #xff #x00 #x00 #xff)))


(defmethod process ((self tracker) (connection null) left right))

(defcmd cmd::tracker ((self app)) (:interactive t)
  (awhen (find-if (lambda (x) (typep x 'tracker)) (.views *app*))
    (remove-view it))
  (let ((tracker (make-instance 'tracker
                                :tracks (loop for x in (.views *app*)
                                              if (typep x 'pattern-module)
                                                collect x))))
    (append-view tracker)
    (setf (.selected-modules *app*) (list tracker))))
