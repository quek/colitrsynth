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
                (unless (= (midi-in-get-dev-caps i caps (cffi:foreign-type-size 'tag-midi-in-caps))
                           +mmsyserr-noerror+)
                  (error "midi-in-get-dev-caps"))
                (cffi:with-foreign-slots ((sz-pname) caps (:struct tag-midi-in-caps))
                  (cffi:foreign-string-to-lisp sz-pname))))
;;⇒ ("FL STUDIO FIRE" "ESI MIDIMATE eX" "MIDIIN2 (ESI MIDIMATE eX)")


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




#+nil
(cffi:with-foreign-object (phandle '(:pointer HMIDIIN))
  (print (midi-in-open phandle 0 (cffi:null-pointer) (cffi:null-pointer) CALLBACK_NULL))
  (let ((handle (cffi:mem-aref phandle 'HMIDIIN 0)))
    (print handle)
    (print (midi-in-close handle))))
