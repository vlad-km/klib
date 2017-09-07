;;; -*- mode:lisp;  coding:utf-8 -*-

;;;; Tenv (Test environment) - addon Moren IDE
;;;; Execute a set of test units (batches)
;;;;
;;;;
;;;; Original idea: David Vázquez Púa
;;;;                JSCL project
;;;;                https://github.com/jscl-project/jscl/blob/master/tests.lisp
;;;;
;;;; Modification: mvk, 2017
;;;;
;;;; Macro names "test", "equal-test" , "expected-failure" ,compared with the original version
;;;; left unchangedare for compatibility with existing JSCL tests cases.
;;;;
;;;; Added:
;;;;
;;;;    Feature downloading source lisp code.  When loading, the code is compiled
;;;;    by the current JSCL environment and executed locally.
;;;;
;;;;    Combining several units into a logical group - batch.
;;;;    For each batch, the execution statistics are separately taken into account
;;;;
;;;;    Interception of error exceptions.
;;;;    Functions "error", "warn" can be used in the body of a unit.
;;;;
;;;;    Separate time accounting
;;;;
;;;; Status:  Sketch
;;;;
;;;; Use
;;;;
;;;; 1. Addon Moren IDE. File: tenv.js, tenv-macro.lisp. Separated load as resource from ide console, or programm
;;;;
;;;; 2. For compilation and execution of a separate application without Moren/environment (pure JSCL).
;;;;    Files: tenv.lisp, tenv-macro.lisp.
;;;;
;;;;
;;;; Have a fun
;;;;



(defparameter *total-tests* 0)
(defparameter *passed-tests* 0)
(defparameter *failed-tests* 0)
(defparameter *expected-failures* 0)
(defparameter *expected-passes* 0)
(defparameter *test-seqn* 1000)
(defparameter *timestamp* nil)
(defparameter *execute-timer* nil)
(defparameter *batch-stop-time* 0)


(defparameter *passed-msg* #("Passed" "Passed expectedly!"))
(defparameter *failed-msg* #("Failed" "Failed expectedly"))


;;;
;;; Load test batch (source lisp file with test forms)
;;;
;;; Before use as console command, "tenv-macro.lisp" will be loaded.
;;;
;;;
;;; (tenv/load "static/js/case-one.lisp")
;;;
;;; macro form: ???
;;;
;;; If you use the tenv/load its necessary to know that this is an assynchronous process
;;; (i.e. compilation of the unit body and its execute)
;;;
;;;

(defun tenv/eval (input)
    (let* ((forms (read-from-string (concat "(" input ")"))))
        (dolist (x forms)
            (eval x))) )

(export '(tenv/load))
(defun tenv/load (name)
    (xhr-receive  name
                  (lambda (input)
                      (tenv/eval (substitute #\Space (code-char 13) input) ))
                  (lambda (uri status)
                      (format t "~%Load: Can't load ~s. Status: ~a~%" uri status)) )
    (values-list nil))



;;; Batch begin
;;;
;;; Set timestump for current test batch
;;; macro form: (batch-begin)
;;;

(export '(tenv/timestamp))
(defun tenv/timestamp ()
    (setq *timestamp* (get-internal-real-time))
    (setq *execute-timer* nil)
    (setq *test-seqn* 1000)
    (setq *total-tests* 0)
    (setq *passed-tests* 0)
    (setq *expected-failures* 0)
    (setq *unexpected-passes* 0))


;;;
;;; Worker
;;;
;;; Todo: Not the best idea to use sexpr as unit descriptor
;;;

;;; reduce long sexpr for print
;;; something like "xxx xxx xxxxxx" => "xxxx ... xxx"
;;;
(defun tenv/censor (sexpr &optional (maxlen 80) )
    (let* ((print-form (write-to-string sexpr))
           (len (length print-form))
           (head-tail 0) (tail-head 0))

        (if (> len maxlen)
            (progn
                (setq head-tail (truncate (* (truncate (* len 0.8)) 0.7)))
                (setq tail-head (truncate (* (truncate (* len 0.8)) 0.25)))
                (concat (string-slice print-form 1 head-tail) " ... " (string-slice print-form (- tail-head))))
            print-form)))


(export '(tenv/worker))
(defun tenv/worker (closure sexpr expected-flg)
    (let ((start-time (get-internal-real-time))
          (end-time 0)
          (elapsed 0)
          (secs 0)
          (result))

        (incf *total-tests*)
        (format t "<font color='yellow'>Test [~d]</font><font color='white'> ~s</font>"
                *total-tests* (tenv/censor sexpr))
        (setq start-time (get-internal-real-time))
        (handler-case
            (progn
                (setf result (funcall closure)
                      end-time (get-internal-real-time)
                      elapsed (- end-time start-time)
                      secs (/ elapsed internal-time-units-per-second 1.0))

                ;;(format t "~%<font color='yellow'>     Start ~a End ~a Elapsed ~a</font>"
                ;;  start-time end-time (- end-time start-time))

                (push (- end-time start-time) *execute-timer*)
                (cond
                  (result
                   (format t "~%    <font color='yellow'> ~a</font>~%" (aref *passed-msg* expected-flg ))
                   (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
                   (incf *passed-tests*))
                  (t (if (= expected-flg 1) (incf *expected-failures*) (incf *expected-passes*))
                     (warn (aref *failed-msg* expected-flg )))))
          ;;
          ;; The warning catches two cases
          ;; - When a unit is used function warn for break from test-unit with the relevant message
          ;; - When the unit returned a value that did not match the expected value
          ;;
          (warning (msg)
              (format t "~%    <font color='orange'> Warning: ~s</font>~%" (!condition-args msg) )
              (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
              (incf *failed-tests*))
          ;;
          ;; Something went wrong
          ;;
          (error (msg)
              (setf end-time (get-internal-real-time)
                    elapsed (- end-time start-time)
                    secs (/ elapsed internal-time-units-per-second 1.0))
              (incf *failed-tests*)
              (format t "~%    <font color='red'> Error: ~s.</font>~%" (!condition-args msg))
              (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
              ))
        ))


;;;
;;; tenv/closing
;;; Close current test batch and print batch finally statistic
;;; macro form (batch-end)
;;;

(export '(tenv/closing))

(defun tenv/closing ()
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
;;;

(addon-provide :tenv)

(/debug "Tenv loaded")

;;; EOF
