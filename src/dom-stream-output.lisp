;;; Klib
;;; DOM output stream
;;;
;;;
;;;
;;;
;;;

#|

HOW USE

(setf *bord (make-wpborder :radius "5px" :color "white"))
(setf *back (make-wpbackground :color "blue"))
(setf *vp (viewport-create :left 500 :border *bord :background *back :color "white" :parent (dom-get-body) :drag t))
(setf *out (make-dom-output-stream :exists (viewport-div *vp)))

(dotimes (i 20)
    (format *out "~a:Hello! <br> <font color='orange'>How are you?</font><p>~d</p><br>" i))

(dotimes (i 40)
    (write-char "s" *out))

|#


;;;
;;; MAKE-OUTPUT-STREAM
;;;
;;; name - dom element id
;;; exists - existing dom element
;;;
;;;
;;; (setf *out (make-dom-output-stream))   ;; create stream *out
;;; (dom-add-class *out "report-style")    ;; add style class
;;; (dom-mount *div-report                 ;; mount *out to current viewport
;;;    (dom-output-stream *out))
;;; (format *out "This is a first<br>")    ;; write to stream
;;;  ...
;;;  ...
;;; (dom-output-stream-reset *out)         ;; reset stream
;;;  =>
;;;      clear all inner text
;;;      the next output will begin from first position;;;

(export '(make-dom-output-stream))

(defun make-dom-output-stream (&key name exists (scroll t) (plain t))
    (let ((buffer))

        (if exists
            (setf buffer exists)
            (setf buffer (dom-create (if plain "pre" "div")
                                     (list (cons "id" (if name name (gen-uid "output" "stream")))))))


        (vector 'stream
                ;; write-char
                (lambda (ch)
                    (let* ((span (dom-create "span")))
                        (setf (oget span "innerHTML") (string ch))
                        (funcall ((oget buffer "appendChild" "bind" ) buffer span))))
                ;; write-string
                (lambda (string)
                    (let* ((span (dom-create "span")))
                        (setf (oget span "innerHTML") string)
                        (funcall ((oget buffer "appendChild" "bind" ) buffer span))
                        (if scroll (funcall ((oget buffer "scrollIntoView" "bind") buffer nil))      ) ) )
                'dom-stream
                buffer)))





;;;
;;; DOM-OUTPUT-STREAM
;;;
;;; Return dom-element from stream if exists
;;; or nil otherwise
;;;
;;; (dom-mount div1 (dom-output-stream *out1) (dom-output-stream *out2))
;;; =>
;;;         div1
;;;         |
;;;         |-div-from-out1
;;;         |
;;;         |-div-from-out2
;;;
;;;

(export '(dom-output-stream))
(defun dom-output-stream (stream)
    (if (arrayp stream)
        (aref stream 4)
        nil))


;;;
;;; DOM-OUTPUT-STREAM-OPEN
;;;
;;; (dom-output-stream-open (dom-output-stream stream) parent-div)
;;;

(export '(dom-output-stream-open))
(defun dom-output-stream-open (stream parent)
    (if (arrayp stream)
        (dom-mount parent (aref stream 4))
        nil))


;;;
;;; DOM-OUTPUT-RESET
;;;
;;; stream-reset
;;;
(export '(dom-output-stream-reset))
(defun dom-output-stream-reset (stream)
    (if (arrayp stream)
        (setf (oget  (aref stream 4) "innerHTML") "")
        nil))


;;;
;;; DOM-OUTPUT-STREAM-CLOSE
;;;
(export '(dom-output-stream-close))
(defun dom-output-stream-close (stream)
    (if (arrayp stream)
        (dom-remove (aref stream 4))
        nil))


;;; EOF
