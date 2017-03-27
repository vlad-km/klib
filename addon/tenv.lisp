;;; -*- mode:lisp;  coding:utf-8 -*-


;;;;
;;;; Klib/Moren test environment
;;;;
;;;; Purpose
;;;;
;;;; Execute a set of test units
;;;;
;;;;
;;;; Original idea: David Vázquez Púa
;;;;                JSCL project
;;;;                https://github.com/jscl-project/jscl/blob/master/tests.lisp
;;;;
;;;; Modification: mvk, 2017
;;;;
;;;; Original macro names "test", "equal-test" , "expected-failure" ,are stored
;;;; for compatibility with the JSCL.
;;;;
;;;; Added:
;;;;
;;;;    Feature downloading Lisp code, using the browser functionality.
;;;;    When loading, the code is compiled by the current JSL environment and executed locally.
;;;;
;;;;    Combining several units into a logical group - batch
;;;;    For each batch, the execution statistics are separately taken into account
;;;;
;;;;    Interception of error exceptions. Functions "error", "warn" can be used in the body of a unit.
;;;;
;;;;    Separate time accounting
;;;;
;;;; Status: Development
;;;;
;;;; Used
;;;;
;;;; 1. Addon Moren IDE. Files: tenv.js, tenv-macro.lisp. Separated load as resource from ide console, or programm
;;;; 2. For compilation and execution of a separate application without Moren/environment (pure JSCL). Files: tenv.lisp,
;;;;    tenv-macro.lisp. For execute need to link the module Klib.
;;;;
;;;;
;;;; Have a fun
;;;;




(export '(tenv/test-fn tenv/test-async tenv/load-tb tenv/set-timestamp tenv/reports))


(defparameter *total-tests* 0)
(defparameter *passed-tests* 0)
(defparameter *failed-tests* 0)
(defparameter *expected-failures* 0)
(defparameter *expected-passes* 0)
(defparameter *test-seqn* 1000)
(defparameter *timestamp* nil)
(defparameter *execute-timer* nil)


(defparameter *passed-msg* #("Passed" "Passed expectedly!"))
(defparameter *failed-msg* #("Failed" "Failed expectedly"))


(defun tenv/test-fn (expr form expected-flg)
    (let ((start-time (get-internal-real-time))
          (end-time 0)
          (elapsed 0)
          (secs 0)
          (result))

        (incf *total-tests*)
        (format t "<font color='yellow'>Test [~d]</font><font color='white'> ~S</font>" *total-tests* form)
        (setq start-time (get-internal-real-time))
        (handler-case
            (progn
                (setf result (funcall expr)
                      end-time (get-internal-real-time)
                      elapsed (- end-time start-time)
                      secs (/ elapsed internal-time-units-per-second 1.0))

                ;;(format t "~%<font color='yellow'>     Start ~a End ~a Elapsed ~a</font>" start-time end-time (- end-time start-time))

                (push (- end-time start-time) *execute-timer*)
                (cond
                  (result
                   (format t "~%    <font color='yellow'> ~a</font>~%" (aref *passed-msg* expected-flg ))
                   (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
                   (incf *passed-tests*))
                  (t (if (= expected-flg 1) (incf *expected-failures*) (incf *expected-passes*))
                     (warn (aref *failed-msg* expected-flg )))))
          (warning (msg)
              (format t "~%    <font color='orange'> Warning: ~s</font>~%" (jscl::!condition-args msg) )
              (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
              (incf *failed-tests*))
          (error (msg)
              (setf end-time (get-internal-real-time)
                    elapsed (- end-time start-time)
                    secs (/ elapsed internal-time-units-per-second 1.0))
              (incf *failed-tests*)
              (format t "~%    <font color='red'> Error: ~s.</font>~%" (jscl::!condition-args msg))
              (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
              ))
        ))


(defun tenv/set-timestamp ()
    (setq *timestamp* (get-internal-real-time))
    (setq *execute-timer* nil)
    (setq *test-seqn* 1000)
    (setq *total-tests* 0)
    (setq *passed-tests* 0)
    (setq *expected-failures* 0)
    (setq *unexpected-passes* 0))

(defparameter *batch-stop-time* 0)

(defun tenv/reports ()
    (setq *batch-stop-time* (get-internal-real-time))
    (format t "~%<font color='yellow'>Batch finished. Total ~a tests</font>~%" *total-tests*)
    (format t "<font color='yellow'>The execution time ~a seconds</font>~%"
            (/ (- *batch-stop-time* *timestamp*) internal-time-units-per-second 1.0))
    (format t "<font color='yellow'>Elapsed time ~a seconds</font>~%"
            (/ (sum *execute-timer*) internal-time-units-per-second 1.0))

    (if (= *passed-tests* *total-tests*)
        (format t "<font color='yellow'>All the tests (~a) passed successfully.</font>~%" *total-tests*)
        (format t "<font color='yellow'>~a/~a test(s) passed successfully.</font>~%" *passed-tests* *total-tests*))

    (unless (zerop *expected-failures*)
        (format t "<font color='orange'>~a test(s) failed expectedly.</font>~%" *expected-failures*))

    (unless (zerop *unexpected-passes*)
        (format t "<font color='yellow'>~a test(s) passed unexpectedly.</font>~%" *unexpected-passes*))

    ;;(format t "~a~%" *execute-timer*)
    (format t "Done~%" ))


;;;
;;; Load-tb
;;;
;;; Load test batch (source lisp file with test forms)
;;;
;;; (load-tb "static/js/integers.lisp")
;;;
;;;

(defun tenv/eval-test-form (input)
    (let* ((forms (read-from-string (concat "(" input ")"))))
        (dolist (x forms)
            (eval x))) )


(defun tenv/load-tb (host-file-name)
    (xhr-receive  host-file-name
                  (lambda (input)
                      (tenv/eval-test-form (substitute #\Space (code-char 13) input) ))
                  (lambda (uri status)
                      (format t "~%Load: Can't load ~s. Status: ~a~%" uri status)) )
    (values))


;;;
;;;
(addon-provide :tenv)
