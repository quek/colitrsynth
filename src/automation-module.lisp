(in-package :colitrsynth)

(defmethod initialize-instance :after ((self automation-module) &key)
  (unless (slot-boundp self 'lines)
    (setf (.lines self)
          (make-array (.nlines self)
                      :initial-element -1.0
                      :element-type 'single-float)))
  (let ((editor (make-instance 'automation-editor
                               :model self)))
    (setf (.editor self) editor)
    (add-child self editor))
  (resized self))

(defmethod events-at-line-frame ((automation automation-module)
                                 pattern-position start-line start-frame end-line end-frame)
  (loop with lines = (.lines automation)
        for current-line from start-line below (- (.end pattern-position)
                                                  (.start pattern-position))
        for current-frame = start-frame then 0
        with value = (aref lines current-line)
        while (<= current-line end-line)
        if (and (/= value -1.0)
                (or (< start-line current-frame end-line)
                    (and (= start-line current-line end-line)
                         (<= start-frame current-frame)
                         (< current-frame end-frame))
                    (and (= start-line current-line)
                         (<= start-frame current-frame))
                    (and (= current-frame end-line)
                         (< current-frame end-frame))))
          collect value))

(defmethod keydown ((self automation-module) value scancode mod-value)
  (unless (keydown (.editor self) value scancode mod-value)
    (call-next-method)))

(defmethod process ((self automation-module) (connection null) left right))

(defmethod render :before ((self automation-module) renderer)
  (update-labels (.editor self)))

(defmethod render ((self automation-module) renderer)
  (call-next-method))

(defmethod resized ((self automation-module))
  (let ((editor (.editor self)))
   (setf (.x editor) *layout-space*
         (.y editor) (+ *font-size* (* *layout-space* 2))
         (.width editor) (- (.width self) 10)
         (.height editor) (- (.height self) (+ 15 *font-size*)))))

(defmethod serialize ((self automation-module))
  `((setf (.edit-step (.editor x)) ,(.edit-step (.editor self)))
    ,@(call-next-method)))
