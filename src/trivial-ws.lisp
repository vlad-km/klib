;;; -*- mode:lisp; coding:utf-8 -*-


;;; Trivail Websocket API for JSCL
;;;
;;; Release: Pre-0.1
;;;
;;;
;;;
;;; Copyleft 2016-2017, MVK
;;;
;;;
;;;


;;;
;;; open socket
;;;      url - "ws://127.0.0.1:port-number" (string)
;;;      message  - websocket onmessage function
;;;      error    - fn oneror
;;;      open     - fn open
;;;      close    - fn close
;;;      protocol - string
;;;
;;; return websocket instance
;;;
;;; (setf ws (tws-open :message (lambda (data)
;;;                               (let* ((msg (#j:window:JSON:parse (oget data "data"))))
;;;                               ...))
;;;                    :open #'open-fn
;;;                    :error #'error-fn
;;;                    :close #'close-fn
;;;                    :protocol "one" ))
;;;
;;; message => (lambda (MessageEvent) ...)
;;;             MessageEvent.data  - msg received
;;;
;;; protocol => Optional. Either a single protocol string or an array of protocol strings.
;;;             These strings are used to indicate sub-protocols,
;;;             so that a single server can implement multiple WebSocket sub-protocols
;;;             (for example, you might want one server to be able to handle different types
;;;             of interactions depending on the specified protocol).
;;;             If you don't specify a protocol string, an empty string is assumed.
;;;             (see MDN WebSocket API)
;;;
;;;             On hunchensoket see hunchensocket::websocket-request
;;;                       headers-in (:sec-websocket-protocol "...")
;;;
;;; url format (1) "ws://127.0.0.1:9999"
;;;            (2) "ws://127.0.0.1:9999/Dom"
;;;            (3) "ws://127.0.0.1:9999/Dom?md=1"
;;;
;;;     (2) - hunchensocket::websocket-request script-name = /Dom
;;;     (3) - hunchensocket::websocket-request
;;;                        query-string = "md=1"
;;;                        get-parameters = (("md" . 1))
;;;
;;; Return
;;;         => ws instance
;;;
(export '(tws-open))
(defun tws-open (&key (url "ws://127.0.0.1:40000") message open close error protocol)
    (if (not message)
        (error "Trivial-ws: onmessage handler must be"))
    (let* ((ws))
        (if protocol
            (setf ws (jscl::make-new #j:WebSocket (jscl::lisp-to-js url) (jscl::lisp-to-js protocol)))
            (setf ws (jscl::make-new #j:WebSocket (jscl::lisp-to-js url) )))

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
;;;
;;; Close code see MDN CloseEvent.code for WebSocket API
;;;
(export '(tws-close))
(defun tws-close (ws &optional (code 1000))
    (funcall ((oget ws "close" "bind") ws code))
    ws )

;;;
;;; Send message to socket
;;;
;;; data must be string
;;;  for example: (#j:window:JSON:stringify message)
;;;   or (write-to-string message)
;;;   or "ping pong"
;;;   or #(1 2 3 )
;;;
;;; => none

(export '(tws-send))
(defun tws-send (ws data)
    (cond ((= (oget ws "readyState") 1)
           (funcall ((oget ws "send" "bind") ws data)))
          (t nil))
    (values-list nil))



;;;
;;; install :trivial-ws-feature *features*
;;;
;;; Another addon should check *features* and make a decision about the continuation
;;; with the issuance of diagnostics
;;;
(addon-provide :trivial-ws-feature :release (list 'Pre-0.1 'pre-0.1))

;;; EOF
