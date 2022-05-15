;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq pipe (sb-win32::create-named-pipe "\\\\.\\pipe\\pluin-host"
                                        sb-win32::pipe-access-duplex
                                        sb-win32::pipe-type-byte
                                        255 0 0 100 (cffi-sys::null-pointer)))
(setf io (sb-sys:make-fd-stream pipe :input t :output t :element-type 'unsigned-byte))
(loop repeat 10 do (read-byte io))
(loop for i from 1 below 11 do (write-byte i io))
(force-output io)
(close io)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((x "C:/Users.../plugin_host.dll"))
  (sb-alien:load-shared-object x)
  (unwind-protect
       (let ((host (sb-alien:alien-funcall
                    (sb-alien:extern-alien "start" (function sb-alien:system-area-pointer)))))
         (unwind-protect
              (let ((plugin
                      (sb-alien:alien-funcall
                       (sb-alien:extern-alien "load" (function sb-alien:system-area-pointer
                                                               sb-alien:system-area-pointer sb-alien:integer))
                       host 0)))
                (sb-alien:alien-funcall
                 (sb-alien:extern-alien "edit" (function sb-alien:void
                                                         sb-alien:system-area-pointer))
                 plugin))
           (when host
             (sleep 10)
             (sb-alien:alien-funcall
              (sb-alien:extern-alien "end" (function sb-alien:void sb-alien:system-area-pointer))
              host))))
    (sb-alien:unload-shared-object x)))


