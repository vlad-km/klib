;;; -*- mode:lisp; coding:utf-8 -*-


;;; JSOM - js-object manipulation
;;;
;;; Release: Pre-0.1
;;; Version: Alpha-0.1
;;;
;;; Inspired by Helmut Kian commit on 13 Jun 2015
;;;             https://github.com/helmutkian/jscl/commit/409add7cbb83a095f15036c4c4879e9c14c768e5
;;;
;;; Copyleft, 2016-2017 mvk
;;;
;;; Tested: Chrome
;;;         Win 7/ ccl v1.11-64
;;;         JSCl master branch
;;;
;;; It requires additional testing
;;;



(export '(make-js-object
          map-js-object
          get-js-object-keys
          get-js-object-values
          get-js-object-pair
          js-object-to-alist))

;;; make-js-object
;;;
;;;
;;; (make-js-object "name" "val" "next" (make-js-object "name2" "val2"))
;;; => {name: "val", next: {name2: "val2"}}
;;;

#|
(defun make-js-object (&rest kv)
    (let* ((obj (new))
           (idx 0)
           (key-val))
        (if (oddp (length kv))
            (error "make-js-object: length of the arguments list not even - ~a" (length kv) ))
        (mapcar (lambda (el)
                    (cond ((oddp idx)
                           (setf (oget obj key-val) el))
                          (t (setf key-val el) ))
                    (incf idx)) kv)
        obj))
|#

(defun make-js-object (&rest kv)
    (let* ((obj (new))
           (idx 0)
           (key-val))
        (if (oddp (length kv))
            (error "make-js-object: length of the arguments list not even - ~a" (length kv) ))
        (map nil (lambda (el)
                     (cond ((oddp idx)
                            (setf (oget obj key-val) el))
                           (t (setf key-val el) ))
                     (incf idx)) kv)
        obj))


;;; set-object-values
;;;
;;; (setf obj (set-object-values (new)
;;;               (cons "value" 1) (cons "func" (lambda (x) (1+ val)))))
;;;  => {value: 1, func:(function)}
;;;
;;; Problem: need js object ["string"] or {urls:["string"]}
;;;          with jscl:
;;;                     (defparameter arr (make-array 1 :initial-element nil))
;;;                     (setf (aref arr 0) "string")
;;;                     => #("string")
;;;                     but on js its look as: Array[Array[6]]. "string" as [s,t,r,i,n,g]
;;;
;;; Workaround
;;;
;;;     (setf arr (#j:makeArray))
;;;     (set-object-values arr (cons 0 (#j:returnString "string")))
;;;
;;;     on js side:  ["string"]
;;;
;;;
;;; other hand
;;;
;;;    (setf obj (set-object-values (new) (cons "fn" "name") (cons "arg" "111") (cons "val" 112)))
;;;    on js: {fn: "name",arg:"111",val:112}
;;;
;;; Ok
;;;
;;; Detail see on klib/lib/rtjscl.js
;;;
(export '(set-object-values))
(defun set-object-values (obj &rest conses)
    (map nil (lambda (el) (#j:seteObjectValue obj (car el) (cdr el))) conses)
    obj)



;;; with-js-object
;;;
;;; js object iterator macro form
;;;
;;; (setf obj (make-js-object "aaa" 111 "bbb" 222))
;;; (oget obj "aaa") => 111
;;;
;;; (setf place '())
;;;
;;; (with-js-object ((key val) obj)
;;;      (push (cons key val) place))
;;; => (("bbb" . 222) ("aaa" . 111))
;;;
;;; (setf temp (aref key-form 0)
;;;       key-val (funcall ((oget temp "toString" "bind") temp)))
;;;
;;; LOCAL MACRO DEFINITION
;;;
(defmacro with-js-object ((binding-form js-object &optional result-form) &body body)
    (destructuring-bind (key-var &optional value-var)
        (if (listp binding-form) binding-form (list binding-form))
        (let ((keys (gensym "keys"))
              (i (gensym "i"))
              (key-ref (gensym "key-ref"))
              (len (gensym "len")))
            `(let* ((,keys (#j:Object:keys ,js-object))
                    (,len (length ,keys)))
                 (dotimes (,i ,len ,result-form)
                     (let* ((,key-ref (aref ,keys ,i))
                            (,key-var (funcall ((oget ,key-ref "toString" "bind") ,key-ref)))
                            ,@(when value-var `((,value-var (oget ,js-object ,key-var)))))
                         ,@body))))
        ))

;;; map-js-object
;;;
;;; js object iterator function form
;;;
;;; (map-js-object #'(lambda (x y) (print x)) obj)
;;; "bbb"
;;; "aaa"
;;;
;;;
(defun map-js-object (fn js-object)
    (with-js-object ((key value) js-object)
        (funcall fn key value)))

;;; get-js-object-keys
;;;
;;; Return list keys for object
;;; (get-js-object-keys obj)
;;; => ("bbb" "aaa")
;;;
(defun get-js-object-keys (js-object)
    (let* ((js-object-keys))
        (with-js-object ((obj-key obj-val) js-object js-object-keys)
            (push obj-key js-object-keys))))


;;; get-js-object-values
;;;
;;; Return list values for object
;;; (get-js-object-values obj)
;;; => (222  111)
;;;
(defun get-js-object-values (js-object)
    (let* ((js-object-values))
        (with-js-object ((obj-key obj-val) js-object js-object-values)
            (push obj-val js-object-values))))


;;; get-js-object-pair
;;;
;;; (values (get-object-keys) (get-object-values))
(defun get-js-object-pair (js-object)
    (values (get-js-object-keys js-object)
            (get-js-object-values js-object)))


;;; js-object-to-alist
;;;
;;; js object key/value mapping
;;;
;;; (js-object-to-alist obj)
;;; => (("bbb" 222) ("aaa" 111))
;;;
;;; (js-object-to-alist obj :make-fn #'cons)
;;; => (("bbb" . 222) ("aaa" . 111))
;;;
(defun js-object-to-alist (js-object &key (make-fn #'list))
    (let ((alist '()))
        (with-js-object ((js-key js-value) js-object alist)
            (push (funcall make-fn js-key js-value) alist))))

;;;; EOF
