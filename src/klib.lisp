(/debug "Klib...")

;;;(unless #j:opNew
;;;    (#j:console:log "klib runtime not present")
;;;    (error "klib runtime not present") )

(addon-provide :klib)

;;;(/debug "provide")

;;;
;;; Export jscl symbols
;;;
;;;

(export  '(oget new make-new fset def!struct
           concat lisp-to-js  js-to-lisp js-null-p join
           %js-try))

(/debug "DONE")
;;; EOF
