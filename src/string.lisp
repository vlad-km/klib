;;; Klib
;;; Wrapper for JS String functions
;;;
;;;
;;;

;;; toLowerCase() / toUpperCase()
;;;
(export '(string-to-lower-case))
(defun string-to-lower-case (src)
    (let* ((jsrc (lisp-to-js src)))
        (funcall ((oget jsrc "toLowerCase" "bind") jsrc))))

;;; toUpperCase
;;;
(export '(string-to-upper-case))
(defun string-to-upper-case (src)
    (let* ((jsrc (lisp-to-js src)))
        (funcall ((oget jsrc "toUpperCase" "bind") jsrc))))

;;; indexOf
;;;
;;; => number index. (if eql -1, then no find)
;;;
(export '(string-index-of))
(defun string-index-of (src pat)
    (let* ((jsrc (lisp-to-js src)))
        (funcall ((oget jsrc "indexOf" "bind") jsrc pat))))


;;; substring(start [, end])
;;;
;;; => string
;;;
(export '(string-substring))
(defun string-substring (src start end)
    (let* ((jsrc (lisp-to-js src)))
        (funcall ((oget jsrc "substring" "bind") jsrc start end))))


;;; substr(start [, length])
;;;
;;; => string
(export '(string-substr))
(defun string-substr (src start len)
    (let* ((jsrc (lisp-to-js src)))
        (funcall ((oget jsrc "substr" "bind") jsrc start len))))

;;; slice(start [, end])
;;;
;;; => string
;;;
(export '(string-slice))
(defun string-slice (src start &optional end)
    (let* ((jsrc (lisp-to-js src)))
        (if end (funcall ((oget jsrc "slice" "bind") jsrc start end))
            (funcall ((oget jsrc "slice" "bind") jsrc start)))))


;;; split(delim max)
;;;
;;; NOte:
;;;      delimiter by default #\space
;;;      maximum elts - 100
;;;
;;; => #( sting string string)
;;;
(export '(string-split))
(defun string-split (src &optional (delim #\space) (max 100))
    (let* ((jsrc (lisp-to-js src)))
        (map 'vector (lambda (x) (js-to-lisp x))
             (funcall ((oget jsrc "split" "bind") jsrc delim max)))))


;;; Global RegExp
;;;
;;; (reg-exp " {2,}")
;;; => #<JS-OBJECT / {2,}/>
;;;
;;; For regexp syntax detail, see:
;;;           https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/RegExp
;;;
;;; (string-replace "aaa bbb       ccc       ddd" (reg-exp " {2,}") " ")
;;; 'aaa            bbb   ccc                    ddd teee fff'.replace(/ {2,}/g,' ')
;;;
;;; (reg-exp " {2,}" "g")  ; Note:
;;; => "aaa bbb ccc ddd"   ;  replace double space from string
;;;                        ;  work only electron
;;; (reg-exp "\s+" "g")    ;  work on Chrome & electron
;;;
;;;
;;; (sting-split (string-replace *str (reg-exp " {2,}" "g") " ") )
;;; => split string without empty elements
;;;

(export '(reg-exp))
(fset 'Reg-Exp #j:RegExp)

;;; replace
;;;
;;;

(export '(string-replace))
(defun string-replace (src pat1 pat2)
    (let* ((j-src (lisp-to-js src)))
        (funcall ((oget j-src "replace" "bind") j-src pat1 pat2))))


;;; EOF
