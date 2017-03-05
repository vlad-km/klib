;;; -*- mode:lisp;  coding:utf-8 -*-

;;;
;;; Common Lisp function 'load'
;;;
;;; Release for jscl
;;;
;;; Copyleft, 2016, mvk
;;; As is, with no guarantees
;;;
;;; (load fname)
;;; 
;;; 1. file with name 'midagi.lisp' contain lisp definition as usual
;;;
;;;     (defparameter *ara (make-array 5 :initial-element 0)
;;;     (defun fun1 () (print 'Im-fun1))
;;;     (defun fun2 (x) (identity x))
;;;  
;;; CL-USER>(load "j/midagi.lisp")
;;; =>
;;; *ARA
;;; FUN1
;;; FUN2
;;; Finalyze
;;;
;;; Note:
;;;
;;;    1. At this place, "j/" - directory inside root chrome extension directory, or route for
;;;       you webserver.
;;;    2. In extension mode, not need use webserver for accessed to local resources. 
;;;       All resources should placed inside directory where from the extension installed   
;;;
;;; Important:
;;;
;;;    Behavior of a function compiled via compile-application (from bootstrap) and compiled when 
;;;    downloading the source code in the browser may vary considerably. 
;;;    You are warned.
;;;

(export '(xhr-receive))
(defun xhr-receive (uri fn-ok &optional fn-err)
    (let* ((req (#j:opNew #j:window "XMLHttpRequest")))
        (funcall ((oget req "open" "bind") req "GET" uri t))
        (funcall ((oget req "setRequestHeader" "bind") req "Cache-Control" "no-cache"))
        (funcall ((oget req "setRequestHeader" "bind") req "Cache-Control" "no-store"))
        (setf (oget req "onreadystatechange")
              (lambda (evt)
                  (if (= (oget req "readyState") 4)
                      (if (= (oget req "status") 200)
                          (progn
                              (funcall fn-ok (oget req "responseText")))
                          (if (not (null fn-err))
                              (funcall fn-err uri (oget req "status") )
                              (#j:console:log (concat "\n" (oget req "status") ": Error load " name " - " (oget req "statusText"))))))))
        (#j:reqXHRsendNull req) ))

(defun eval-form (input)
    (%js-try
     (handler-case
         (progn
             (let* ((forms (read-from-string (concat "(" input ")")))
                    (result nil))
                 (dolist (x forms)
                     (setf result (multiple-value-list (eval-interactive x)))
                     (dolist (y result)
                         (format t "~a ~%" y)))))
       (warning (msg)
           (format t "Warning: ~s~%" (!condition-args msg)))
       (error (msg)
           (format t "Error: ~s~%" (!condition-args msg))))
     (catch (err)
         (format t "Catch js error: ~s~%" (or (oget err "message") err)))
     (finally
      (format t "Finalyze~%")) ))

(export '(load))
(defun load (host-file-name)
    (xhr-receive  host-file-name
                  (lambda (input)
                      (eval-form (substitute #\Space (code-char 13) input) ))
                  (lambda (uri status)
                      (format t "~%Load: Can't load ~s. Status: ~a~%" uri status)) )
    (values))
