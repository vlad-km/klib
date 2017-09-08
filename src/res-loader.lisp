;;; -*- mode:lisp;  coding:utf-8 -*-

;;;
;;; Klib
;;; Resource loader (js & css) function
;;;
;;;
;;
;;;


;;;
;;; Load rx.js from http://localhost/???/vendor/
;;;
;;;         (resource-loader :script "/vendor/rx.js")
;;;
;;; Load css file from http://localhost/???/css/
;;;
;;;         (resource-loader :css "/css/calendar.css")
;;;
;;;

(export '(resource-loader))
(defun resource-loader (what from &optional onload)
    (let* ((link nil)
           (src nil))
        (case what
          (:script
           (setq link (dom-create "script" (list (cons "type" "text/javascript" )
                                                 (cons "src" from)))))
          (:css
           (setq link (dom-create "link" (list (cons "rel" "stylesheet")
                                               (cons "type" "text/css")
                                               (cons "href" from)))))
          (otherwise (error "Resource-loader: what load? ~a~%" what)))

        (when onload
            (dom-set-event link "onload"
                           (lambda (evt)
                               (if (equal what :script) (dom-remove link))
                               (funcall onload from))))
        (dom-append *dom-body* link)
        (unless onload (if (equal what :script) (dom-remove link)))))

;;; EOF
