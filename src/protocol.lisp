(in-package :colitrsynth)

(defgeneric initialize (module))

(defgeneric render (self renderer)
  (:method (self renderer)))

(defgeneric resized (self)
  (:method (self)))

(defgeneric mousebuttondown (self button state clicks x y)
  (:method (self button state clicks x y)
    (setf (click-target-module button) self)
    (swhen (.focused-view *app*)
      (unless (and (<= (.render-x it) (.mouse-x *app*) (+ (.render-x it) (.width it)))
                   (<= (.render-y it) (.mouse-y *app*) (+ (.render-y it) (.height it))))
        (setf it nil)))))

(defgeneric mousebuttonup (self button state clicks x y)
  (:method (self button state clicks x y)
    (when (eq self (click-target-module button))
      (let* ((root (.root-parent self))
             (x (- (.mouse-x *app*) (.render-x root)))
             (y (- (.mouse-y *app*) (.render-y root))))
        (case clicks
          (2 (double-click root button x y))
          (t (click root button x y)))))))

(defgeneric click (self button x y)
  (:method (self button x y)))

(defgeneric double-click (self button x y)
  (:method (self button x y)))

(defgeneric drag-start (self x y button)
  (:method (self x y button)))

(defgeneric drag (self xrel yrel button)
  (:method (self xrel yrel button)))

(defgeneric drag-end (self x y button)
  (:method (self x y button)))

(defgeneric drop (self dropped x y button)
  (:method (self dropped x y button)))

(defgeneric mousemotion (self x y xrel yrel state)
  (:method (self x y xrel yrel state)))

(defgeneric keydown (self value scancode mod-value)
  (:method (self value scancode mod-value)
    (awhen (gethash *current-key* *app-keymap*)
      (funcall it *app*))
    t))

(defgeneric keyup (self value scancode mod-value)
  (:method (self value scancode mod-value)))

(defgeneric move (self xrel yrel)
  (:method (self xrel yrel)))

(defgeneric resize (self xrel yrel)
  (:method (self xrel yrel)))

(defgeneric focused (self)
  (:method (self)))

(defgeneric lost-focuse (self)
  (:method (self)))

(defgeneric wheel (self delta)
  (:method (self delta)))

(defgeneric .target (self)
  (:method ((self null)) nil))

(defgeneric .x (self))
(defgeneric .render-x (self))
(defgeneric .screen-x (self))
(defgeneric .render-center-x (self))
(defgeneric .screen-center-x (self))
(defgeneric .y (self))
(defgeneric .render-y (self))
(defgeneric .screen-y (self))
(defgeneric .render-center-y (self))
(defgeneric .screen-center-y (self))
