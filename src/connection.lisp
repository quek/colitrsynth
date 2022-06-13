(in-package :colitrsynth)

(defmethod .name ((self midi-connection))
  "In MIDI")

(defmethod .name ((self audio-connection))
  (format nil "In ~d" (.dest-bus self)))

(defmethod .name ((self builtin-param-connection))
  (builtin-parameter-name (.param self)))

(defmethod .name ((self plugin-param-connection))
  (plugin-parameter-name (.param self)))

(defmethod connection-eql (a b)
  (and (eql (type-of a) (type-of b))
       (eql (.src a) (.src b))
       (eql (.dest a) (.dest b))))

(defmethod connection-eql ((a audio-connection) (b audio-connection))
  (and (call-next-method)
       (= (.src-bus a)
          (.src-bus b))
       (= (.dest-bus a)
          (.dest-bus b))))

(defmethod connection-eql ((a builtin-param-connection) (b builtin-param-connection))
  (eql (builtin-parameter-accessor (.param a))
       (builtin-parameter-accessor (.param b))))

(defmethod connection-eql ((a plugin-param-connection) (b plugin-param-connection))
  (= (plugin-parameter-index (.param a))
     (plugin-parameter-index (.param b))))

(defmethod connect (connection)
  (push connection (.out (.src connection)))
  (push connection (.in (.dest connection))))

(defmethod connected-p (connection)
  (member connection (.in (.dest connection))
          :test #'connection-eql))

(defmethod disconnect (connection)
  (setf (.out (.src connection))
        (remove connection (.out (.src connection))
                :test #'connection-eql))
  (setf (.in (.dest connection))
        (remove connection (.in (.dest connection))
                :test #'connection-eql)))

(defmethod disconnect-all ((self model))
  (loop for connection in (copy-list (.in self))
        do (disconnect connection))
  (loop for connection in (copy-list (.out self))
        do (disconnect connection)))


(defgeneric render-connection (self r)
  (:method (self r)))

(defmethod render-connection ((self view) r)
  (loop for child in (.children self)
        do (render-connection child r))
  (call-next-method))

(defmethod render-connection ((self connector) renderer)
  (when (eq (.module self) (aif (.cable-src *app*)
                                (.src it)))
    (apply #'sdl2:set-render-draw-color renderer *connection-line-color*)
    (multiple-value-bind (x y) (sdl2:mouse-state)
      (sdl2:render-draw-line renderer
                             (.screen-center-x self)
                             (.screen-center-y self)
                             x y)))
  (call-next-method))

(defmethod render-connection ((self connector-mixin) r)
  (loop with map = (make-hash-table)
        for connection in (.out self)
        for offset = (let ((count (incf (gethash (.dest connection) map -1))))
                       (if (evenp count)
                           (* (/ count 2) -8)
                           (* (ceiling (/ count 2)) 8)))
        do (multiple-value-bind (xs ys xe ye)
               (compute-connection-points self (.dest connection) offset)
             (let ((original-xs xs)
                   (original-ys ys))
               (when (typep self 'track-view)
                 (let ((partial-view (.parent self)))
                   (setf xs (min (max xs (.render-x partial-view))
                                 (+ (.render-x partial-view)
                                    (.width partial-view))))
                   (setf ys (min (max ys (.render-y partial-view))
                                 (+ (.render-y partial-view)
                                    (.height partial-view))))))
               (apply #'sdl2:set-render-draw-color r (cable-color connection))
               ;; (sdl2:render-draw-line r xs ys xe ye)
               (draw-cable r connection xs ys xe ye)
               (when (and (= xs original-xs) (= ys original-ys))
                 (apply #'sdl2:set-render-draw-color r *connection-point-color*)
                 (sdl2:render-fill-rect r
                                        (sdl2:make-rect (- xs 3) (- ys 3) 7 7))))))
  (call-next-method))

(defmethod draw-cable (renderer
                       connection
                       x1 y1 x2 y2)
  (multiple-value-bind (left-buffer right-buffer)
      (cable-buffer (.src connection) connection)
    (if left-buffer
        (let* ((distance (distance x1 y1 x2 y2))
               (buffer-length (min distance (length left-buffer)))
               (scale (/ distance buffer-length))
               (aux-points nil)
               (points (loop for i from 0 below buffer-length
                             for left = (aref left-buffer i)
                             for right = (if right-buffer (aref right-buffer i) 0)
                             for value = (+ left right)
                             for x = i
                             for y-i-1 = 0 then y
                             for y = (* value 10)
                             nconc (cons
                                    (cons (* x scale) y)
                                    (loop for i from (min y y-i-1) below (max y y-i-1)
                                          collect (cons (* x scale) i)))
                             finally (setf aux-points
                                           (loop for i from (min y 0) to (max y 0)
                                                 collect (cons (* x scale) i)))))
               (points (append aux-points points))
               (rad (radian x1 y1 x2 y2))
               (points (loop for (x . y) in points
                             for x% = (- (* x (cos rad)) (* y (sin rad)))
                             for y% = (+ (* x (sin rad)) (* y (cos rad)))
                             collect (cons (round (+ x% x1)) (round (+ y% y1)))))
               (points (loop for (x . y) in points
                             collect (sdl2:make-point x y))))
          (multiple-value-bind (points num) (apply #'sdl2:points* points)
            (sdl2:render-draw-points renderer points num)))
        (sdl2:render-draw-line renderer x1 y1 x2 y2))))

(defmethod cable-buffer (module connection)
  (values nil nil))
