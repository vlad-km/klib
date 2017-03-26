;;; -*- mode:lisp; coding:utf-8 -*-
;;;
;;; Klib
;;; JSOM - js-object manipulation functions
;;;
;;; Copyright (C) 2017 mvk (github.com/vlad-km)
;;;
;;;
;;;
;;;
;;; Release: Pre-0.2
;;;
;;; Tested: Chrome/56.0 (extension)
;;;         Electron



;;; make-js-object
;;;
;;;
;;; (make-js-object "name" "val" "next" (make-js-object "name2" "val2"))
;;; => {name: "val", next: {name2: "val2"}}
;;;

(export '(make-js-object))
(defun make-js-object (&rest kv)
    (let* ((obj (new))
           (idx 0)
           (key-val))
        (if (oddp (length kv))
            (error "make-js-object: length of args not even - ~a" (length kv) ))
        (map nil (lambda (el)
                     (cond ((oddp idx)
                            (setf (oget obj key-val) el))
                           (t (setf key-val el) ))
                     (incf idx)) kv)
        obj))


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
(export '(get-js-object-keys))
(defun get-js-object-keys (js-object)
    (map 'list #'(lambda (x) (js-to-lisp x)) (#j:Object:keys js-object)))


;;; js-object-to-list
;;;
;;; => ("aa" 1 "bb" 2)
;;;

(export '(js-object-to-list))
(defun js-object-to-list (obj)
    (mapcar #'(lambda (x) (list x (oget obj x)))
            (map 'list #'(lambda (x) (js-to-lisp x)) (#j:Object:keys obj))))

;;;; EOF
