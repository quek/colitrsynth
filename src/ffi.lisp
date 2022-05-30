(in-package :colitrsynth.ffi)

(cffi:load-foreign-library "comdlg32.dll")

(cffi:defctype WORD :short)
(cffi:defctype DWORD :unsigned-long)
(cffi:defctype HWND :pointer)
(cffi:defctype HINSTANCE :pointer)
(cffi:defctype LPCTSTR (:pointer :char))
(cffi:defctype LPTSTR (:pointer :char))
(cffi:defctype LPARAM :unsigned-long)
(cffi:defctype PVOID :pointer)
(cffi:defctype LPVOID :pointer)
(cffi:defctype LPOFNHOOKPROC :pointer)
(cffi:defctype BOOL :int32)

(cffi:defcstruct open-file-name
  (lStructSize DWORD)
  (hwndOwner HWND)
  (hInstance HINSTANCE)
  (lpstrFilter LPCTSTR)
  (lpstrCustomFilter LPTSTR)
  (nMaxCustFilter DWORD)
  (nFilterIndex DWORD)
  (lpstrFile LPTSTR)
  (nMaxFile DWORD)
  (lpstrFileTitle LPTSTR)
  (nMaxFileTitle DWORD)
  (lpstrInitialDir LPCTSTR)
  (lpstrTitle LPCTSTR)
  (flags DWORD)
  (nFileOffset WORD)
  (nFileExtension WORD)
  (lpstrDefExt LPCTSTR)
  (lCustData LPARAM)
  (lpfnHook LPOFNHOOKPROC)
  (lpTemplateName LPCTSTR)

  (pvReserved LPVOID)
  (dwReserved DWORD)
  (flagsEx DWORD))

(defconstant OFN_FILEMUSTEXIST #x1000)
(defconstant OFN_NOCHANGEDIR #x8)
(defconstant OFN_PATHMUSTEXIST #x800)

(cffi:defcfun ("GetOpenFileNameA" %get-open-file-name) BOOL
  (param (:pointer (:struct open-file-name))))

(cffi:defcfun ("GetSaveFileNameA" %get-save-file-name) BOOL
  (param (:pointer (:struct open-file-name))))

(defun get-open-file-name ()
  (cffi:with-foreign-objects ((param '(:struct open-file-name)))
    (loop for i below (cffi:foreign-type-size '(:struct open-file-name))
          do (setf (cffi:mem-aref param :int8) 0))
    (cffi:with-foreign-strings ((file (make-string 1024))
                                (filter (format nil "lisp~c*.lisp~cAll~c*.*~c"
                                                #\null #\null #\null #\null))
                                (title "select file")) 
      (cffi:with-foreign-slots ((lStructSize hwndOwner lpstrFile nMaxFile lpstrFilter nFilterIndex
                                             lpstrFileTitle flags)
                                param (:struct open-file-name))
        (setf lStructSize (cffi:foreign-type-size '(:struct open-file-name)))
        (setf hwndOwner (cffi:null-pointer))
        (setf lpstrFile file)
        (setf nMaxFile 1024)
        (setf lpstrFilter filter)
        (setf nFilterIndex 1)
        (setf lpstrFileTitle title)
        (setf flags (logior OFN_FILEMUSTEXIST OFN_NOCHANGEDIR OFN_PATHMUSTEXIST)) 

        (if (zerop (%get-open-file-name param))
            nil
            (cffi:foreign-string-to-lisp lpstrFile :encoding :CP932))))))
;;(get-open-file-name)

(defun get-save-file-name ()
  (cffi:with-foreign-objects ((param '(:struct open-file-name)))
    (loop for i below (cffi:foreign-type-size '(:struct open-file-name))
          do (setf (cffi:mem-aref param :int8) 0))
    (cffi:with-foreign-strings ((file (make-string 1024))
                                (filter (format nil "lisp~c*.lisp~cAll~c*.*~c"
                                                #\null #\null #\null #\null))) 
      (cffi:with-foreign-slots ((lStructSize hwndOwner lpstrFile nMaxFile lpstrFilter nFilterIndex
                                             lpstrFileTitle flags)
                                param (:struct open-file-name))
        (setf lStructSize (cffi:foreign-type-size '(:struct open-file-name)))
        (setf hwndOwner (cffi:null-pointer))
        (setf lpstrFile file)
        (setf nMaxFile 1024)
        (setf lpstrFilter filter)
        (setf nFilterIndex 1)
        (setf flags (logior OFN_NOCHANGEDIR OFN_PATHMUSTEXIST)) 

        (if (zerop (%get-open-file-name param))
            nil
            (cffi:foreign-string-to-lisp lpstrFile :encoding :CP932))))))
;;(get-save-file-name)
