(in-package :sdl2-ttf)

(defmacro define-render-wrapped-function (style encoding)
  (let* ((foreign-function-name (format 'nil "TTF_Render~a_~a_Wrapped" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style "-wrapped"))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(define-function ,foreign-function-name ,wrapper-function-name ,low-level-lisp-name
         :pointer
         ((font :pointer) (text :string) (color (:struct sdl-color)) (wrap-length :uint32))
         (font text wrap-length red green blue alpha)
       (autocollect (ptr)
           ;;We need to wrap this manually since we are providing the function ourselves
           (check-null (sdl2-ffi::make-sdl-surface
                        :ptr (,low-level-lisp-name (autowrap:ptr font)
                                                   text
                                                   (create-sdl-color-list red
                                                                          green
                                                                          blue
                                                                          alpha)
                                                   wrap-length)))
         (sdl-free-surface ptr)))))

(define-render-wrapped-function "Solid" "UTF8")


