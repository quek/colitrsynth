(in-package :colitrsynth)

(defmethod serialize ((self column))
  `(make-instance
    'column
    ,@(when (/= (.note self) none) `(:note ,(.note self)))
    ,@(when (/= (.velocity self) *default-velocity*) `(:velocity ,(.velocity self)))
    ,@(when (/= (.delay self) 0) `(:delay ,(.delay self)))))
