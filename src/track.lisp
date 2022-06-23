(in-package :colitrsynth)

(defmethod available-cables-src ((src track-view))
  (cons (make-instance 'midi-connection :src src :dest nil)
        (aif (find-if  (lambda (x)
                         (typep (.pattern x) 'automation-module))
                       (.pattern-positions src))
             (cons (make-instance 'param-connection
                                  :src src
                                  :dest nil
                                  :param it) ;TODO どうしよう
                   nil))))

(defmethod route-connection ((connection midi-connection)
                             (src track)
                             dest
                             midi-events-param-events start-frame)
  (process dest
           connection
           (car midi-events-param-events)
           start-frame))

(defmethod route-connection ((connection param-connection)
                             (src track)
                             dest
                             midi-events-param-events start-frame)
  (process dest
           connection
           (cdr midi-events-param-events)
           start-frame))
