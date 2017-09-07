;;; -*- mode:lisp;  coding:utf-8 -*-

;;;
;;; Part of the addon Moren IDE - Tenv
;;;
;;; see tenv.lisp for details
;;;
;;; Tenv API
;;;

;;;
;;; (test (= (progn 1) 1))
;;; (test (let () t))
;;; (test (warn "Message"))
;;;
;;; May be use format on body test unit
;;;
;;;   (test
;;;      (let ()
;;;       (format t ...)
;;;        (dotimes ...) t))
;;;
;;;
;;; (defun midagi ()
;;;     (batch-begin "Midagi batch")
;;;
;;;     (format t "Stage 1")
;;;     (test ....)
;;;
;;;     (format t "Stage end")
;;;     (test ...)
;;;     (batch-end) )
;;;
;;; (midagi)
;;;
;;;


;;;
;;; (test something there)
;;;
(defmacro test (condition)
    `(tenv/test-fn (lambda () ,condition) ',condition  0))

;;;
;;; (expected-failure (fail))
;;;
(defmacro expected-failure (condition)
    `(tenv/test-fn (lambda () ',condition) ',condition 1))

;;;
;;; (test-equal (gethash frob::key frob::ht) 1234)
;;;
(defmacro test-equal (form value)
    `(tenv/test (equal ,form ,value )))

;;;
;;; Batch-begin
;;;
;;; Recomended begining for any test batch
;;;
(defmacro batch-begin (header)
    (declare (ignore header))
    `(tenv/set-timestamp))

;;;
;;; Batch-end
;;;
;;; Print run-time statistic for the test batch
;;;
(defmacro batch-end ()
    `(tenv/reports))


;;;
;;; test-load name
;;;
(defmacro test-load (name)
    `(tenv/load-tb ,name))
