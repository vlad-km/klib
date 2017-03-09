;;; -*- mode:lisp; coding:utf-8 -*-


;;; Trivail Websocket API for JSCL
;;;
;;; Release: Pre-0.1
;;;
;;;
;;;
;;; Copyleft 2016, MVK
;;;
;;; Depends: klib/src/rtjscl.js (opNew)
;;;
;;;


;;;
;;; open socket
;;;      url - "ws://127.0.0.1:port-number" (string)
;;;      onmsg  - websocket onmessage function
;;;      error - oneror
;;;      open  - opem
;;;      close - close
;;; return websocket instance
;;;
;;; (setf ws (tws-open :onmsg (lambda (data)
;;;                            (let* ((msg (#j:window:JSON:parse (oget data "data"))))
;;;                               ...))
;;;                    :open #'open-ffn
;;;                    :error #'error-fn ))
;;;
;;; message => (lambda (MessageEvent) ...)
;;;             MessageEvent.data  - msg receved
;;;
;;; => ws instance

(export '(tws-open))
(defun tws-open (&key (url "ws://127.0.0.1:40000") message open close error)
    (if (not message)
        (error "Trivial-ws: onmessage handler must be"))
    (let* ((ws (#j:opNew #j:window "WebSocket" url)))
        (when ws
            (setf (oget ws "onmessage") message)
            (if open (setf (oget ws "onopen") open))
            (if close (setf (oget ws "onclose") close))
            (if error (setf (oget ws "onerror") error)))
        ws))


;;;
;;; Close socket
;;;
;;; => ws instance
;;;
(export '(tws-close))
(defun tws-close (ws)
    (funcall ((oget ws "close" "bind") ws))
    ws )

;;;
;;; Send message to socket
;;;
;;; data must be string
;;;  for example: (#j:window:JSON:stringify message)
;;;   or (write-to-string message)
;;;   or "ping pong"
;;;
;;; => none

(export '(tws-send))
(defun tws-send (ws data)
    (cond ((= (oget ws "readyState") 1)
           (funcall ((oget ws "send" "bind") ws data)))
          (t nil))
    (values-list nil))

;;; EOF
