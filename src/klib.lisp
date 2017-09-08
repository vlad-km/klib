;;; -*- mode:lisp; coding:utf-8 -*-

;;;
;;; Klib
;;;

;;;
;;; addon *features* support
;;;
;;; Another addon should check *features* and make a decision about the continuation
;;; with the issuance of diagnostics
;;;
;;; :release may be list => '(pre-01 pre-02 pre-0.1)
;;;
;;; (addon-provide :tws '(v1 prot-0))
;;; (addon-provide :tws 'v1)
;;;
(defun addon-provide (feature &key release)
    (let ((f (find feature *features*))
          (r (if release (ensure-list release) nil)))
        (cond (f
               (when r
                   (setq plist (symbol-plist f))
                   (if plist
                       (setf (getf plist :relese) (list :release r))
                       (setf (symbol-plist f) (list :release r)))))
              (t
               (setq f feature)
               (if r (setf (symbol-plist f) (list :release r)))
               (push f *features*)))))

(export '(addon-provide))


;;;
;;; (addon-requiere :trivial-ws :release 'pre-02)
;;; if existing  ws addon has release pre-01, will be raise error
;;;
;;;
;;; and form (addon-require '(:tws (v1 prot-1 send-0) '(:sock (v1 pre-01))))
;;; or form  (addon-require '(:tws v1))
;;;
(defun addon-require (&rest reqp)
    (dolist (r reqp)
        (if (consp r)
            (addon-require-1 (car r) :release (cadr r))
            (addon-require-1 r) ))
    t)

(export '(addon-require))


(defun addon-require-1 (feature &key release)
    (let ((present (find feature *features*)))
        (unless present
            (error (concat  " :" (symbol-name feature) " it bug, but not feature")))
        (let* ((plist (symbol-plist present))
               (rel-present (getf plist :release))
               (msg))
            (when release
                (cond ((symbolp release)
                       (if (find release rel-present)
                           (return-from addon-require-1 t)
                           (error (concat "Feature " (symbol-name feature) " not " release))))
                      ((consp release)
                       (dolist (r release)
                           (if (find r rel-present)
                               t
                               (error (concat "Feature " (symbol-name feature) " not " r)))))
                      (t (error "wtf feature release ~a ?" release)))
                t))))



;;;
;;; Export jscl symbols
;;;
;;;


(export  '(oget new make-new fset def!struct
           concat lisp-to-js  js-to-lisp js-null-p join
           %js-try))


(export '(aset
          oset
          ensure-list
          storage-vector-set storage-vector-ref storage-vector-p
          list-to-vector vector-to-list
          sequencep))



(addon-provide :klib)


;;; EOF
