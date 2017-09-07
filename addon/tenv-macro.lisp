;;; -*- mode:lisp;  coding:utf-8 -*-

;;;
;;; Part of the addon Moren IDE - Tenv
;;;
;;; see tenv.lisp for details
;;;
;;; Tenv macro forms for API
;;;

;;;
;;; (test (= (progn 1) 1))
;;; (test (let () t))
;;;
;;; May be use format on body test unit
;;; in this case, skip six spaces before own msg for to save the basic format
;;;   (test
;;;      (let ()
;;;       (format t "      ~a ..." n )
;;;        (dotimes ...) t))
;;;
;;;
;;; (defun midagi ()
;;;     (batch-begin "Midagi batch")
;;;
;;;     (format t "Stage 1")
;;;     (test ....)
;;;
;;;     (format t "Stage 1 end")
;;;     (test ...)
;;;     (batch-end) )
;;;
;;; (midagi)
;;;
;;;


;;;
;;; (test something there)
;;;
(defmacro test (expr)
    `(tenv/worker (lambda () ,expr) ',expr  0))

;;;
;;; (expected-failure (fail))
;;;
(defmacro expected-failure (expr)
    `(tenv/worker (lambda () ,expr) ',expr 1))

;;;
;;; (test-equal (gethash frob::key frob::ht) 1234)
;;;
;;; Note: Not some good.
;;;       Rather so:  (test (= expr expeced))
;;;
(defmacro test-equal (expr expected)
    `(tenv/worker (lambda () (equal ,expr ,expected )) ',expr 0))

;;;
;;; Batch-begin
;;;
;;; Recomended begining for any test batch
;;;
;;; (batch-begin)
;;;   (test )
;;;   (test)
;;;   (test)
;;; (batch-end)
;;;

(defmacro batch-begin (header)
    (declare (ignore header))
    `(tenv/timestamp))

;;;
;;; Batch-end
;;;
;;; Print run-time statistic for the test batch
;;;
(defmacro batch-end ()
    `(tenv/closing))


;;;
;;; test-load name
;;;
;;; Load lisp source file with test definition
;;;
(defmacro test-load (name)
    `(tenv/load ,name))
