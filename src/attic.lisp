;;;
;;;  Any useful, partially snatch from various places
;;;


;;; (true)/(fail)/(nop)
;;;
;;;        (if (true) (true) (fail))
;;;        (if (fail) (fail) (true))
;;;
;;; (true "It's right way")
;;; (fail "Goddamn what this")
;;; (nop  "Sometimes nop its just nop. Sorry for my asm style")
;;;
(export '(true fail nop))

(defun true (&rest any)
    (declare (ignore any))
    t)

(defconstant fail nil)

(defun fail (&rest any)
    (declare (ignore any))
    fail)

(defun nop (&rest any)
    (declare (ignore any)))


;;;
;;; addon *features* support
;;;
;;; Another addon should check *features* and make a decision about the continuation
;;; with the issuance of diagnostics
;;;
;;; :release may be list => '(pre-01 pre-02 pre-0.1)
;;;
(export '(addon-provide))
(defun addon-provide (feature &key release)
    (unless (member feature *features*)
        (push feature *features*))
    (if release
        (setf (symbol-plist feature) (list 'release release)))
    (values-list nil) )


;;;
;;; (addon-requiere :trivial-ws :release 'pre-02)
;;; if existing  ws addon has release pre-01, will be raise error
;;;
(export '(addon-require))
(defun addon-require (feature &key release)
    (unless (member feature *features*)
        (error (concat "Add " feature )))
    (when release
        (let* ((fea (get feature 'release))
               (msg (format nil "Need ~a ~a. Present ~a~%" feature release fea ) ))
            (typecase fea
              (cons
               (if (not (member release fea))
                   (error msg)))
              (symbol (if (not (equal fea release))
                          (error msg))))))
    (values-list nil))



;;;
;;; gen-uid
;;;
;;; Generate uniq id (mostly for dom elements id)
;;;
;;;  prefix-(uniq-name from gensym)-postfix
;;;
;;; (gen-uid "image" "stuff" )
;;;   => "image-stuff35736-id"
;;;
(export '(gen-uid))
(defun gen-uid (prefix name &optional (postfix "-id"))
    (format nil "~a-~a~a" prefix (string (gensym name)) postfix))



;;;
;;; Split string
;;;
;;; (split-str (list #\space #\. ) "aaaa bb.bbbb cccc")
;;; => ("aaaa" "bb" "bbbb" "cccc")
(export '(split-str))
(defun split-str (chars str &optional (lst nil) (accm ""))
    (cond
      ((= (length str) 0)
       (reverse
        (cond ((and (equal accm "") (null lst)) '())
              ((equal accm "") lst)
              (t (cons accm lst)))))
      (t
       (let ((c (char str 0)))
           (if (member c chars)
               (split-str chars (subseq str 1) (if (equal accm "")
                                                   lst
                                                   (cons accm lst)) "")
               (split-str chars (subseq str 1)
                          lst
                          (concat accm (string c)))) ))))


;;;
;;; Split string
;;;
;;;
;;; (split "aaa bbb ccc" )
;;;      => ("aaa" "bbb" "ccc")
;;;
;;; (split "aaa bbb ccc" :max 2)
;;;      => ("aaa" "bbb ccc")
;;;
;;; (split "aaa  bbb ccc")
;;;      => ("aaa" "" "bbb" 'ccc)
;;;
;;; (split "aaa  bbb ccc" :not-empty t)
;;;      => ("aaa" "bbb" "ccc")
;;;
(export '(split))
(defun split (src-str &key not-empty  max (delim '(#\Space #\Tab)))
    (flet ((delim-p (char) (find char delim)))
        (reverse
         (let ((result nil)
               (start 0)
               (words 0)
               (end 0)
               (item))
             (loop
               (when (and max (>= words (1- max)))
                   (return (cons (subseq src-str start) result)))
               (setf end (position-if #'delim-p src-str :start start))
               (setf item (subseq src-str start end))
               (cond ((and not-empty (> (length item) 0))
                      (push item result)
                      (incf words))
                     ((not not-empty)
                      (push item result)
                      (incf words)))
               (unless end (return result))
               (setf start (1+ end)))))))


;;;
;;; (pair '("a" "b" 'c") '(1 2 3))
;;;       => (("a" . 1) ("b" . 2) ("c" . 3))
(export '(pair))
(defun pair (k v)
    (cond ((and k v)
           (cons (cons (car k) (car v)) (pair (cdr k) (cdr v))))
          ((or k v)
           (error "pair:Length lists in pair"))
          (t nil)))


;;; From "On Lisp"
(export '(mkstr))
(defun mkstr (&rest args)
    "Make a string out of the printed representations of the arguments." ; LMH
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

;;; From "On Lisp"
(export '(symb))
(defun symb (&rest args)
    "Make a symbol out of the printed representations of the arguments." ; LMH
    (values (intern (apply #'mkstr args rest2))))



;;;; Other utils from auxfns paip


(export '(starts-with punctuation-p print-with-spaces mappend not-null
          first-or-nil first-or-self))

(defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (equal (first list) x)))


(defun punctuation-p (char)
    (find char ".,;:`!?#-()\\\""))

(defun print-with-spaces (list)
    (mapc #'(lambda (x) (prin1 x) (princ " ")) list))



(defun mappend (fn &rest lists)
    "Apply fn to each element of lists and append the results."
    (apply #'append (apply #'mapcar fn lists)))


(defun not-null (x) (not (null x)))


(defun first-or-nil (x)
    "The first element of x if it is a list; else nil."
    (if (consp x) (first x) nil))


(defun first-or-self (x)
    "The first element of x, if it is a list; else x itself."
    (if (consp x) (first x) x))


;;; rest from rest (i.e. without the first two)
;;; (rest2 '(1 2 3 4 5))
;;; => (3 4 5)
;;;
(export '(rest2))
(defun rest2 (form)
    (rest (rest form)))



;;;; from PATTERN MATCHING FACILITY


(export '(flatten mklist mappend2 random-elt sum))

(defun flatten (the-list)
    "Append together elements (or lists) in the list."
    (mappend2 #'mklist the-list))

(defun mklist (x)
    "Return x if it is a list, otherwise (x)."
    (if (listp x)
        x
        (list x)))

(defun mappend2 (fn the-list)
    "Apply fn to each element of list and append the results."
    (apply #'append (mapcar fn the-list)))


(defun random-elt (choices)
    "Choose an element from a list at random."
    (elt choices (random (length choices))))


(defun sum (values)
    (reduce #'+ values))



;;;; Randoms

(export '(random))
(defun random (n)
    (#j:Math:floor (* (#j:Math:random) n)))


;;;; EOF
