;;; -*- mode:lisp; coding:utf-8  -*-

;;;
;;; This file is part of the KLIB
;;;
;;; implementation of descriptors D/ O/B  function FORMAT
;;;
;;; format-d
;;; format-octal
;;; format-hexal
;;; format-binary
;;;
;;;



;;;
;;; DIGITS-SPLIT-BY-GROUP
;;;
;;; Group the digits of a string for commas
;;;
;;;   digits   - string digits "12345"
;;;   group    - split by groups if not nil
;;;   comma    - comma character
;;;
;;; (digits-split-by-group "1234" 3 #\,)
;;; (digits-split-by-group "+1234" 3 #\,)
;;; (digits-split-by-group "-1234" 3 #\,)
;;;
;;;  => "1,234"
;;;     "+1,234"
;;;     "-1,234"
;;;

(defun %digits-split-by-group (digits group comma)
    (if (find (char digits 0) '(#\+ #\-))
        (concat (char digits 0)
                (%digits-split-by-group (subseq digits 1) group comma))
        (let* ((from (reverse digits))
               (len (length from))
               (result nil)
               (idx 1))
            (push (char from 0) result)
            (while (< idx len)
                (when (zerop (mod idx group))
                    (push comma result))
                (push (char from idx) result)
                (incf idx))
            (apply 'concat result))))

;;; INT-STR
;;;
;;; Integer to string convertor
;;;
;;; x     intVal
;;; sign  need + sign if x positive
;;;

(defun %int-str-d (x sign)
    (if (zerop x)
        "0"
        (let* ((n (make-new #j:Number x))
               (str (funcall ((oget n "toString" "bind") n) 10)))
            (if (and sign (plusp x))
                (concat #\+ str)
                str))))
;;;
;;; ~D format
;;;
;;; ~[SignMod]mincols,padchar,commaChar,commaInterval[GroupMod]D
;;;
;;;  SignMod  := @
;;;  GroupMod := :
;;;
;;; ~d     -> (format-d intval)
;;;          => 123
;;;
;;; ~@d    -> (format-d intval :sign t)
;;;          => +123
;;;
;;; ~@:d   -> (format-d intval :sign t :group t)
;;;          => +1 234
;;;        -> (format-d intval :sign t :group t :comma #\,)
;;;          => +1,234
;;;
;;; ~12,,,d  -> (format-d intval :mincols 12)
;;;          => "         123"
;;; ~@12d    -> (format-d intval :mincols 12 :sign t)
;;;          => "        +123"
;;;
;;;
;;; Example:
;;;
;;;        (format-d -123 :sign t :mincols 5)
;;;        => " -123"
;;; or
;;;        (format t "~a | ~a~%" (format-d -123 :sign t :mincols 5) (format-d -2281 :sign t :mincols 5) )
;;;        =>
;;;            -123 | -2281
;;;
(defun format-d (value &key mincols (padchar #\space) sign group (comma #\space) (interval 3))
    (let ((str nil))
        (setq str (%int-str-d value sign))
        (if group
            (setq str (%digits-split-by-group str interval comma)))
        (if mincols
            (let ((strlen (length str)))
                (if (> mincols strlen)
                    (concat (make-string (- mincols strlen) :initial-element padchar) str)
                    str))
            str )))

(export '(format-d))

;;; Others radix

(defun %format-numeric (value radix &key mincols (padchar #\space))
    (let* ((n (make-new #j:Number value))
           (str (funcall ((oget n "toString" "bind") n radix))))
        (if mincols
            (let ((strlen (length str)))
                (if (> mincols strlen)
                    (concat (make-string (- mincols strlen) :initial-element padchar) str)
                    str))
            str)))

;;; OCTAL
(defun format-octal (value &key mincols (padchar #\space))
    (%format-numeric value 8 :mincols mincols :padchar padchar))

(export '(format-octal))



;;; HEXAL
(defun format-hex (value &key mincols (padchar #\space))
    (%format-numeric value 16 :mincols mincols :padchar padchar))

(export '(format-hexal))



;;; BINARY
(defun format-binary (value &key mincols (padchar #\space))
    (%format-numeric value 2 :mincols mincols :padchar padchar))


(export '(format-binary))



;;; EOF
