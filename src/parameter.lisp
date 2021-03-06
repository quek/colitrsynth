(in-package :colitrsynth)

(defparameter *frames-per-buffer* 512)
(defparameter *sample-rate* 48000.0)
;; (defparameter *sample-rate* 44100.0)

(defparameter *default-velocity* #x64)

(defparameter *transparency* #xc0)
(defparameter *default-color* (list #xdd #xdd #xdd *transparency*))
(defparameter *cursor-color* (list #x00 #x00 #xcc *transparency*))
(defparameter *selection-color* (list #x00 #x44 #x77 *transparency*))
(defparameter *background-color* (list #x00 #x00 #x00 *transparency*))
(defparameter *play-position-color* (list #x00 #x80 #x00 *transparency*))

(defparameter *index-color* (list #xee #xee #xee *transparency*))
(defparameter *note-color* (list #xee #xee #xee *transparency*))
(defparameter *velocity-color* (list #xee #xbb #x55 *transparency*))
(defparameter *delay-color* (list #xee #x66 #xee *transparency*))
(defparameter *insert-mode-color* (list #x00 #x33 #x33 *transparency*))

(defparameter *connection-line-color* (list #x22 #x8b #x22 *transparency*))
(defparameter *cable-color-audio-aux* (list #x8b #x44 #x44 *transparency*))
(defparameter *cable-color-midi* (list #x22 #xaa #xaa *transparency*))
(defparameter *cable-color-param* (list #x22 #x22 #x8b *transparency*))
 
(defparameter *connection-point-color* (list #xff #xff #xff *transparency*))
(defparameter *selected-module-color* (list #xff #xff #x00 *transparency*))
(defparameter *selected-pattern-color* (list #xff #x80 #x80 *transparency*))
(defparameter *focused-color* (list #xff #xff #x22 *transparency*))
(defparameter *mute-color* (list #xff 0 0 *transparency*))
(defparameter *loop-color* (list #x00 #x50 #x20 *transparency*))

(defparameter *solo-background-color* (list #x00 #xcc #x00 *transparency*))
(defparameter *mute-background-color* (list #xcc #x00 #x00 *transparency*))

(defparameter *font-size* 14)
(defparameter *char-width* (/ *font-size* 2))
(defparameter *char-height* *font-size*)
(defparameter *pixcel-per-line* 1)
(defparameter *layout-space* 5)

(defparameter *track-height* 30)        ;TODO 固定長で妥協
