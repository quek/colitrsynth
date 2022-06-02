(in-package :colitrsynth)


(defparameter *transparency* #xc0)

(defparameter *cursor-color* (list #x00 #x00 #xcc *transparency*))
(defparameter *default-color* (list #xdd #xdd #xdd *transparency*))
(defparameter *play-position-color* (list #x00 #x80 #x00 *transparency*))
(defparameter *connection-line-color* (list #x22 #x8b #x22 *transparency*))
(defparameter *connection-point-color* (list #xff #xff #xff *transparency*))
(defparameter *selected-module-color* (list #xff #xff #x00 *transparency*))
(defparameter *selected-pattern-color* (list #xff #x80 #x80 *transparency*))
(defparameter *mute-color* (list #xff 0 0 *transparency*))

(defparameter *font-size* 14)
(defparameter *char-width* (/ *font-size* 2))
(defparameter *char-height* *font-size*)
(defparameter *pixcel-per-line* 1)
(defparameter *layout-space* 5)
