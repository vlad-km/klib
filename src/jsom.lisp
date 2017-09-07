;;; -*- mode:lisp; coding:utf-8 -*-
;;;
;;; Klib
;;; JSOM - js-object manipulation functions
;;;
;;; Copyright (C) 2017 mvk (github.com/vlad-km)
;;;



;;; make-js-object
;;;
;;;
;;; (make-js-object "name" "val" "next" (make-js-object "name2" "val2"))
;;; => {name: "val", next: {name2: "val2"}}
;;;

(defun make-js-obj (&rest kv)
    (let* ((obj (new))
           (idx 0)
           (key-val))
        (if (oddp (length kv))
            (error "make-js-object: length of the arguments list not even - ~a" (length kv) ))
        (dolist (it kv)
            (if (oddp idx)
                (setf (oget obj key-val) it)
                (setq key-val it))
            (incf idx))
        obj))

(fset 'mkjso #'make-js-object)
(export '(make-js-object mkjso))


;;; map-js-object
;;;
;;; js object iterator
;;;
;;; (map-js-object obj  #'(lambda (x y) (print x)) )
;;;
;;; "aaa"
;;; "bbb"
;;;
(export '(map-js-object))
(defun map-js-object (obj fn)
    (mapcar #'(lambda (x) (funcall fn x (oget obj x)))
            (map 'list #'(lambda (x) (js-to-lisp x)) (#j:Object:keys obj))))


;;; get-js-object-keys
;;;
;;; Return list object keys
;;;
;;; => ("bbb" "aaa")
;;;

(defun get-js-object-keys (js-object)
    (map 'list #'(lambda (x) (js-to-lisp x)) (#j:Object:keys js-object)))

(fset 'jso-keys #'get-js-object-keys)
(export '(get-js-object-keys jso-keys))


;;; js-object-to-list
;;;
;;; => ("aa" 1 "bb" 2)
;;;

(export '(js-object-to-list))
(defun js-object-to-list (obj)
    (mapcar #'(lambda (x) (list x (oget obj x)))
            (map 'list #'(lambda (x) (js-to-lisp x)) (#j:Object:keys obj))))



;;;
;;; copies js object
;;; returning a new object with inherite propertiess
;;;

(fset '%jso-assign #j:Object:assign)

(defun jso-copy (obj)
    (#j:Object:assign (new) obj))

(export '(jso-copy))

;;;
;;; Merge few objects to new object
;;;
;;; The identical properties are replaced
;;; See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign
;;; for details
;;;
(defun jso-merge (&rest objs)
    (apply '%jso-assign (new) objs))
(export '(jso-merge))


;;;
;;; delete properties from obj
;;; use (delete-property key obj) from JSCL
;;;


;;;; EOF
