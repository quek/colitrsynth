(in-package :colitrsynth.ffi)

;;; C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\um\mmeapi.h
(cffi:load-foreign-library "winmm.dll")

(cffi:defctype MMRESULT :unsigned-int)
(cffi:defctype HMIDIIN :long)

(cffi:defcfun ("midiInGetNumDevs" midi-in-get-num-devs) :unsigned-int)

#+ni
(midi-in-get-num-devs)
;;⇒ 3

;;; #define MMSYSERR_NOERROR      0                    /* no error */
(defconstant +mmsyserr-noerror+ 0)

;;; #define MAXPNAMELEN      32     /* max product name length (including NULL) */
(defconstant +max-pname-len+ 32)

(cffi:defcstruct tag-midi-in-caps
  (w-mid WORD)
  (w-pid WORD)
  (v-dirver-version :unsigned-int)
  (sz-pname :char :count #.+max-pname-len+)
  (dw-support DWORD))

(cffi:defcfun ("midiInGetDevCapsA" midi-in-get-dev-caps) MMRESULT
  (device-id :unsigned-int)
  (pmic (:pointer (:struct tag-midi-in-caps)))
  (cbmic :unsigned-int))

#+nil
(loop for i below (midi-in-get-num-devs)
      collect (cffi:with-foreign-object (caps '(:struct tag-midi-in-caps))
                (unless (= (midi-in-get-dev-caps i caps (cffi:foreign-type-size '(:struct tag-midi-in-caps)))
                           +mmsyserr-noerror+)
                  (error "midi-in-get-dev-caps"))
                (cffi:with-foreign-slots ((sz-pname) caps (:struct tag-midi-in-caps))
                  (cffi:foreign-string-to-lisp sz-pname))))

(defun midi-device-id (name)
  (loop for i below (midi-in-get-num-devs)
        thereis (cffi:with-foreign-object (caps '(:struct tag-midi-in-caps))
                  (unless (= (midi-in-get-dev-caps i caps (cffi:foreign-type-size '(:struct tag-midi-in-caps)))
                             +mmsyserr-noerror+)
                    (error "midi-in-get-dev-caps"))
                  (cffi:with-foreign-slots ((sz-pname) caps (:struct tag-midi-in-caps))
                    (and (equal (cffi:foreign-string-to-lisp sz-pname)
                                name)
                         i)))))

(defun collect-midi-devices ()
  (loop for i below (midi-in-get-num-devs)
        collect (cffi:with-foreign-object (caps '(:struct tag-midi-in-caps))
                  (unless (= (midi-in-get-dev-caps i caps (cffi:foreign-type-size '(:struct tag-midi-in-caps)))
                             +mmsyserr-noerror+)
                    (error "midi-in-get-dev-caps"))
                  (cffi:with-foreign-slots ((sz-pname) caps (:struct tag-midi-in-caps))
                    (cons i (cffi:foreign-string-to-lisp sz-pname))))))
#+nil
(collect-midi-devices)

#+nil
(midi-device-id "Generic USB MIDI")

;; /* flags used with waveOutOpen(), waveInOpen(), midiInOpen(), and */
;; /* midiOutOpen() to specify the type of the dwCallback parameter. */

;; #define CALLBACK_TYPEMASK   0x00070000l    /* callback type mask */
;; #define CALLBACK_NULL       0x00000000l    /* no callback */
;; #define CALLBACK_WINDOW     0x00010000l    /* dwCallback is a HWND */
;; #define CALLBACK_TASK       0x00020000l    /* dwCallback is a HTASK */
;; #define CALLBACK_FUNCTION   0x00030000l    /* dwCallback is a FARPROC */
;; #ifdef _WIN32
;; #define CALLBACK_THREAD     (CALLBACK_TASK)/* thread ID replaces 16 bit task */
;; #define CALLBACK_EVENT      0x00050000l    /* dwCallback is an EVENT Handle */

(defconstant CALLBACK_TYPEMASK   #x00070000)
(defconstant CALLBACK_NULL       #x00000000)
(defconstant CALLBACK_WINDOW     #x00010000)
(defconstant CALLBACK_TASK       #x00020000)
(defconstant CALLBACK_FUNCTION   #x00030000)
(defconstant CALLBACK_THREAD     CALLBACK_TASK)
(defconstant CALLBACK_EVENT      #x00050000)


(cffi:defcfun ("midiInOpen" midi-in-open) MMRESULT
  (handle (:pointer HMIDIIN))
  (device-id :unsigned-int)
  (callback :pointer)
  (instance :pointer)
  (fdw-open DWORD))

(cffi:defcfun ("midiInClose" midi-in-close) MMRESULT
  (handle HMIDIIN))


(cffi:defcfun ("midiInStart" midi-in-start) MMRESULT
  (handle HMIDIIN))

(cffi:defcfun ("midiInStop" midi-in-stop) MMRESULT
  (handle HMIDIIN))

(cffi:defcfun ("midiInReset" midi-in-reset) MMRESULT
  (handle HMIDIIN))

;;;            /* MIDI input */
(defconstant MM_MIM_OPEN         #x3C1)
(defconstant MM_MIM_CLOSE        #x3C2)
(defconstant MM_MIM_DATA         #x3C3)
(defconstant MM_MIM_LONGDATA     #x3C4)
(defconstant MM_MIM_ERROR        #x3C5)
(defconstant MM_MIM_LONGERROR    #x3C6)

(cffi:defcallback midi-in-callback :void ((handle HMIDIIN)
                                          (msg :unsigned-int)
                                          (instance :pointer)
                                          (param1 :pointer)
                                          (param2 :pointer))
  (declare (ignore instance))
  (case msg
    (#.MM_MIM_OPEN
     (format t "~&MIDI callback open. ~d" handle))
    (#.MM_MIM_CLOSE
     (format t "~&MIDI callback close. ~d" handle))
    (#.MM_MIM_DATA
     (let* ((message (cffi:pointer-address param1))
            (time (cffi:pointer-address param2))
            (event (ash (ldb (byte 4 4) message) 4))
            (channel (ldb (byte 4 0) message))
            (note (ldb (byte 8 8) message))
            (velocity (ldb (byte 8 16) message))
            (midi-event (make-instance 'colitrsynth::midi-event
                                       :event event
                                       :channel channel
                                       :note note
                                       :velocity velocity
                                       :frame time)))
       (format t "~&MIDI callback data. ~d ~x ~x ~a" handle
               (cffi:pointer-address param1)
               (cffi:pointer-address param2)
               midi-event)))
    (#.MM_MIM_LONGDATA
     (format t "~&MIDI callback long data. ~d" handle))
    (#.MM_MIM_ERROR
     (format t "~&MIDI callback error. ~d" handle))
    (#.MM_MIM_LONGERROR
     (format t "~&MIDI callback log error. ~d" handle))))

#|
void CALLBACK MidiInProc(
   HMIDIIN   hMidiIn,
   UINT      wMsg,
   DWORD_PTR dwInstance,
   DWORD_PTR dwParam1,
   DWORD_PTR dwParam2
);
|#


#+nil
(cffi:with-foreign-object (phandle '(:pointer HMIDIIN))
  (print (midi-in-open phandle (midi-device-id "Generic USB MIDI")
                       (cffi:callback midi-in-callback)
                       (cffi:null-pointer) CALLBACK_FUNCTION))
  (let ((handle (cffi:mem-aref phandle 'HMIDIIN 0)))
    (print handle)
    (print (midi-in-start handle))
    (sleep 10)
    (print (midi-in-stop handle))
    (print (midi-in-reset handle))
    (print (midi-in-close handle))))
