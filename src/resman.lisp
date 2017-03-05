;;; -*- mode:lisp;  coding:utf-8 -*-


;;;
;;; Trivial resource manager
;;;
;;; Actually more to work on the console.
;;;
;;; Each object have some name (as string or integer)
;;;
;;; thing with name "answer my question" and forms (dom-create-input ...)
;;;
;;; (res-alloc "answer" (dom-create-input ..))
;;; (res-alloc "Thing" (lambda (x) x))
;;; (funcall (res-refer "thing") 11)
;;;
;;; Important! Do not use in production code!!!
;;;

(export '(res-alloc res-refer res-drop res-script-load res-css-load
          res-image-load res-mount res-remove))


(defparameter *resources* (make-hash-table :test #'equal))


;;;
;;; Because jscl hash-table dont (gethas 1 ht)
(defun ensure-ht-key (nick)
    (cond ((stringp nick) nick)
          ((integerp nick) (string nick))
          (t nick)))

;;;
;;; (res-alloc 1 (list 1 2 3))
;;;
(defun res-alloc (nick what)
    (setf (gethash (ensure-ht-key nick) *resources*) what))

;;;
;;;
;;; (res-refer 1)
;;; => (1 2 3)
;;;
(defun res-refer (nick)
    (gethash (ensure-ht-key nick) *resources*))


;;;
;;; Drop resource
;;;
;;; (setf *img (dom-create "img"))
;;; (setf (oget *img "src") "path/name.jpg")
;;;
;;; if the *img is mounted to the dom structure
;;;
;;;    (dom-mount view *img)
;;;     ...
;;;    (dom-remove *img)
;;;    (setf *img nil)
;;;
;;;
;;;
;;; If *img not in dom structure
;;;   (js-null-p (oget *img "parentNode"))
;;;   (funcall ((oget *img "remove" "bind") *img))
;;;   (setf *img nil)
;;;
;;;
;;;  (setf *img nil) and
;;;     (setf (gethash (ensure-ht-key nick) *resources*) nil)
;;;     (setf elt nil)
;;;  need for correct js garbaged dropped <img> <div> and other
;;;  i.e. any references on this elements will be removed
;;;

#|
(defun res-drop (nick)
    (setf (gethash (ensure-ht-key nick) *resources*) nil)
    (remhash (ensure-ht-key nick) *resources*))
|#

(defun res-drop (nick)
    (let* ((elt (gethash (ensure-ht-key nick) *resources*)))
        (when elt
            (when (and (dom-element-p elt) (js-null-p (oget elt "parentNode")))
                (funcall ((oget elt "remove" "bind") elt)))
            (setf (gethash (ensure-ht-key nick) *resources*) nil)
            (remhash (ensure-ht-key nick) *resources*)
            (setf elt nil))))


;;;
;;; Resource load - css, script, image
;;;
;;;
;;; By default downloaded resource is located in  resman *resource*.
;;; If you overrides "onload", its your task
;;;



;;;
;;; Load js script, and mount her to dom body (not head!)
;;; If necessary, can be removed from the dom structure
;;; (res-script-load "Test FFI")
;;; (res-remove "Test FFI")
;;;

(defun res-script-load (nick from &key (where (dom-get-body)) onload onerror)
    (let* ((link (dom-create "script" (list (cons "type" "text/javascript")))))
        (dom-append where link)
        (dom-set-event link "onload"
                       (lambda (ev)
                           (res-alloc nick link)
                           (if onload
                               (funcall onload))))
        (when onerror
            (dom-set-event link "onerror" onerror))
        (setf (oget link "src") from)
        (values)))



;;;
;;; Load CSS
;;;
(defun res-css-load (nick from &key (where (dom-get-body)) onload onerror)
    (let* ((link (dom-create "link" (list (cons "rel" "stylesheet")
                                          (cons "type" "text/css")))))
        (dom-append where link)
        (dom-set-event link "onload"
                       (lambda (ev)
                           (res-alloc nick link)
                           (if onload
                               (funcall onload))))
        (when onerror
            (dom-set-event link "onerror" onerror))
        (setf (oget link "href") from))
    (values))


;;;
;;; Load Image file
;;;
;;; (res-image-load "CEO photo small" "stuff/officials/small-boss,jpg" :height 100 :width 80)
;;; (res-image-load "CEO photo full" "stuff/officials/small-boss,jpg" )
;;; (res-alloc "All management's galery" (dom-create "div"))
;;; (res-mount (res-refer "All management's galery") "Our CEO photo small" "Our CEO photo full")
;;; (res-mount (dom-get-body) (res-refer "All management's galery"))


;;;
;;; :onload (lambda (evt)
;;;           (let* ((img (get-elt-from-event evt)))
;;;                 (setf (oget img "width" 100)) ... ))
;;;
(defun get-elt-from-event (ev &optional (name "path") (idx 0))
    (aref (oget ev name) idx))

(defun res-image-load (nick from &key height width onload onerror)
    (let* ((uid (gen-uid "resource" "image"))
           (img (dom-create "img" (list (cons "id" uid) ))))
        (if height
            (setf (oget img "height") height))
        (if width
            (setf (oget img "width") width))
        (dom-set-event img "onload"
                       (lambda (ev)
                           (res-alloc nick img)
                           (if onload
                               (funcall onload))))
        (when onerror
            (dom-set-event img "onerror" onerror))
        (setf (oget img "src") from)
        (values)))

;;;
;;; target - any root dom element for append resource with nick name and other childs dom element's
;;;
(defun res-mount (target nick &rest child)
    (apply #'dom-mount (append (list target)
                               (list (res-refer nick))
                               child)))

;;;
;;; remove mounted resource from dom
;;;

#|
(defun res-remove (nick &key drop)
    (let* ((link (res-refer nick)))
        (cond (link
               (dom-remove link)
               (if drop
                   (res-drop nick)))
              (t (error "RES: no such thing ~s" nick)))
        (values)))
|#

(defun res-remove
    (let* ((link (res-refer nick)))
        (unless link (error (format nil "RES: no such thing ~s" nick)))
        (dom-remove link)
        (if drop (res-drop nick))
        (values)))
