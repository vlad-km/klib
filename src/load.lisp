;;; -*- mode:lisp;  coding:utf-8 -*-

;;;
;;; Klib
;;; function 'load'
;;;
;;;


;;;
;;; (load fname)
;;;
;;; 1. file with name 'midagi.lisp' contain
;;;
;;;     (defparameter *ara (make-array 5 :initial-element 0)
;;;     (defun fun1 () (print 'Im-fun1))
;;;     (defun fun2 (x) (identity x))
;;;
;;; CL-USER>(load "j/midagi.lisp")
;;;
;;; Note:
;;;
;;;    1. At this place, "j/" - directory inside root chrome extension directory, or route for
;;;       you webserver.
;;;    2. In extension mode, not need use webserver for accessed to local resources.
;;;       All resources should placed inside directory where from the extension installed
;;;
;;;

(export '(xhr-receive))
(defun xhr-receive (uri fn-ok &optional fn-err)
    (let* ((req (make-new #j:XMLHttpRequest)))
        (funcall ((oget req "open" "bind") req "GET" uri t))
        (funcall ((oget req "setRequestHeader" "bind") req "Cache-Control" "no-cache"))
        (funcall ((oget req "setRequestHeader" "bind") req "Cache-Control" "no-store"))
        (setf (oget req "onreadystatechange")
              (lambda (evt)
                  (if (= (oget req "readyState") 4)
                      (if (= (oget req "status") 200)
                          (funcall fn-ok (oget req "responseText"))
                          (if fn-err
                              (funcall fn-err uri (oget req "status") )
                              (format t "xhr-receive: load error ~a ~a~%" uri (oget req "statusText")))))))
        (#j:reqXHRsendNull req) ))

(defun %%load-form-eval (input &key verbose ready error)
    (let* ((w-err t)
           (form-num 0)
           (forms (read-from-string (concat "(" input ")"))))
        (when verbose
            (format t "Loaded ~d sexpr~%" (length forms))
            (format t "Compilation~%Wait...~%"))
        (%js-try
         (handler-case
             (progn
                 (let* ((form-result nil))
                     (dolist (eform forms)
                         (setq form-result (multiple-value-list (eval eform)))
                         (incf form-num)
                         (when verbose
                             (dolist (x form-result)
                                 (format t "~a ~%" x)  )) )))
           (error (msg)
               (setq w-err nil)
               (format t "Error [~a] : ~s~%" form-num (!condition-args msg))))
         (catch (err)
             (setq w-err nil)
             (format t "<font color='red'>Error [~a]: ~s</font>~%"
                     form-num  (or (oget err "message") err)))
         (finally
          (if verbose (format t "Finallyse~%"))
          (cond (w-err
                 (if ready (funcall ready) (print 'Done)))
                (t
                 (if error (funcall error) (print (concat "Error at " form-num)))))  ))
        (values-list nil)))



(export '(load))
(defparameter *ldr-rpl-patt* (Reg-exp (code-char 13) "g"))

(defun load (name &key (verbose nil) (ready nil) (so-so nil) )
    (xhr-receive  name
                  (lambda (input)
                      (%%load-form-eval
                       (string-replace input *ldr-rpl-patt* " ")
                       :verbose verbose
                       :ready ready
                       :error so-so ))
                  (lambda (uri status)
                      (format t "~%Load: Can't load ~s. Status: ~a~%" uri status)) )
    (values-list nil))


;;; EOF
