(in-package :colitrsynth)

(defmethod initialize-instance :after ((self gain-module) &key)
  (let ((pan (make-instance 'slider
                            :max 1.0
                            :min 0.0
                            :value (lambda () (.pan self))
                            :onchange (lambda (x) (setf (.pan self) x)))))
    (add-child self pan)
    (defmethod resized ((this (eql pan)))
      (let ((x *layout-space*)
            (y (+ (* *font-size* 2) (* *layout-space* 3)))
            (width (- (.width self) (* 2 *layout-space*)))
            (height (+ *font-size* (round (/ *layout-space* 2)))))
        (setf (.x this) x)
        (setf (.y this) y)
        (setf (.width this) width)
        (setf (.height this) height)
        (call-next-method))))
  (resized self))

(defmethod available-connections (src (dest gain-module) (cable-src audio-connection))
  (append (call-next-method)
          (list (make-instance
                 'builtin-param-connection
                 :src src :dest dest
                 :param (make-builtin-parameter :name "Gain"
                                                :accessor '.volume)))))

(defmethod cable-buffer ((module gain-module) (connection audio-connection))
  (values (.left module)
          (.right module)))

(defmethod process-in ((self gain-module) (connection audio-connection) left right)
  (macrolet ((m (op)
               `(loop for i below *frames-per-buffer*
                      with volume = (.volume self)
                      with pan = (.pan self)
                      with pan-right = (* pan 2)
                      with pan-left = (- 2.0 pan-right)
                      do (,op (aref (.left self) i) (* (aref left i) volume pan-left))
                         (,op (aref (.right self) i) (* (aref right i) volume pan-right)))))
    (if (zerop (.in-count self))
        (m setf)
        (m incf))))

(defmethod process-out ((self gain-module))
  (route self (.left self) (.right self)))

(defmethod serialize ((self gain-module))
  `((setf (.volume x) ,(.volume self)
          (.pan x) ,(.pan self))
    ,@(call-next-method)))
