;;;  Klib
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
    (declare (ignore any))
    (values-list nil))


;;; pairs from 2 lists
;;; some as zip lst1 lst2
;;;
(export '(pair))
(defun pair (k v)
    (cond ((and k v)
           (cons (list (car k) (car v)) (pairs (cdr k) (cdr v))))
          ((or k v)
           (error "pairr"))
          (t nil)))

;;; cons pair primitive
;;;     any key-val pair
;;;     any seq as tail head
;;;
;;;   (mkpair :lt 11 :rt 12) => (11 . 12)
;;;   (pair-lt *kv) => (car *kv)
;;;   (pair-rt *kv) => (cdr *kv)
;;;
;;;

(export '(mkpair))
(defun mkpair (&key lt rt)
    (cons lt rt))


(export '(pair-lt))
(defun pair-lt (pair)
    (car pair))


(export '(pair-rt))
(defun pair-rt (pair)
    (cdr pair))



(defun set-pair-lt (pair val)
    (rplaca pair val))

(defun set-pair-rt (pair val)
    (rlpacd pair val))

(export '(set-pair-rt set-pair-lt))


;;; (setf (pair-rt *pp) 999)
(defsetf 'pair-rt set-pair-rt)

;;; (setf (pair-lt *pp) 'qqq)
(defsetf 'pair-lt 'set-pair-lt)






;;;
;;; gen-uid
;;;
;;; Generate uniq id (mostly for dom elements id)
;;;
;;;        prefix-(uniq-name from gensym)-postfix
;;;
;;; (gen-uid "image" "stuff" )
;;;   => "image-stuff35736-id"
;;;
(export '(gen-uid))
(defun gen-uid (prefix name &optional (postfix "-id"))
    (concat prefix "-" (string (gensym name)) postfix))


;;; LISTS

(export '(append1))
(defun append1 (lst obj)
    (append lst (list obj)))

(export '(conc1))
(defun conc1 (lst obj)
    (nconc lst (list obj)))

(export '(mklist))
(defun mklist (obj)
    (if (listp obj)
        obj
        (list obj)))

(export '(single))
(defun single (arg)
    (and (consp arg)
         (null (cdr (the cons arg)))))

(export '(last1))
(defun last1 (lst)
    (car (the cons (last lst))))


;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.

(export '(mkstr))
(defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.

(export '(symb))
(defun symb (&rest args)
    (values (intern (apply #'mkstr args))))


;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.

;;; mvk group source n -> group n source
;;;
(export '(group))
(defun group (n source)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
                 (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons
                                    (subseq source 0 n)
                                    acc))
                         (nreverse
                          (cons source acc))))))
        (if source (rec source nil) nil)))


;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.

(defun flatten (x)
    (labels ((rec (x acc)
                 (cond ((null x) acc)
                       ((atom x) (cons x acc))
                       (t (rec
                           (car x)
                           (rec (cdr x) acc))))))
        (rec x nil)))


;;;
;;; Currying functions
;;;
;;; (mapcar (lambda (base) (expt base 2)) '(2 3 4 5 6))
;;; =>
;;;   (4 8 16 32 64)
;;;
;;; (mapcar (compose (curry #'* 2) (curry #'+ 1)) '(1 2 3 4))
;;; => (4 6 8 10)
;;;
(export '(curry))
(defun curry (function &rest args)
    (lambda (&rest more-args)
        (apply function (append args more-args))))


(export '(rcurry))
(defun rcurry (function &rest args)
    (lambda (&rest more-args)
        (apply function (append more-args args))))



;;;
;;; foldl
;;;
;;; (reduce (lambda (args fun) (print (list fun arg ))) '(f1 f2 f3)  :initial-value 'args)
;;;
;;; (foldl 'function 'args (f1 f2 f3) )
;;; =>
;;; (F1 ARGS)
;;; (F2 (F1 ARGS))
;;; (F3 (F2 (F1 ARGS)))
;;; (F3 (F2 (F1 ARGS)))
;;;
;;;
(export '(foldl))
(defun foldl (fn init seq)
    (reduce fn seq :initial-value init))

;;;
;;; foldr
;;;
;;; (reduce (lambda (fun args) (print (list fun arg ))) '(f1 f2 f3) :from-end t   :initial-value 'args)
;;;
;;; (foldr 'function '(f1 f2 f3) 'args)
;;; =>
;;; (F3 ARGS)
;;; (F2 (F3 ARGS))
;;; (F1 (F2 (F3 ARGS)))
;;; (F1 (F2 (F3 ARGS)))
;;;
(export '(foldr))
(defun foldr (fn seq init)
    (reduce fn seq :from-end t :initial-value init))


;;;
;;; compose functions
;;; right association
;;;
;;; compose g f x
;;; (g ( f (x arg)))
;;;
(export '(compose))
(defun compose (&rest functions)
    (cond ((null functions)   'identity)
          ((single functions) (car functions))
          ((single (rest functions))
           (destructuring-bind (fn1 fn2) functions
               (lambda (&rest args)
                   (funcall fn1 (apply fn2 args))) ))
          (t (let ((fn1 (last1 functions))
                   (fnseq (butlast functions)))
                 (lambda (&rest args)
                     (foldr 'funcall fnseq (apply fn1 args)))
                 ))))

;;;
;;; chain f1 f2 f3
;;; => f1(args).f2().f3()
;;;    f3( f2 (f1 args))
;;;
;;; (chain (curry f1 a1) f2 (curry f3 a1 a2 a3))
;;; funcall * args
;;;
(export '(chain))
(defun chain (&rest x) (apply 'compose (reverse x)))



;;;
;;; combine two funs with binary op
;;;
(export '(combine))
(defun combine (op g f)
    (lambda (args)
        (funcal op (funcall g args) (funcall f args))))




;;; longer
;;; x longer y
;;;
(export '(longer))
(defun longer (x y)
    (> (length x) (length y)))


;;; remove duplicates
;;; dedup
;;;
;;; Not present at JSCL
(export '(rmdup))

(defun rmdup (lst)
    (cond ((eq lst nil) '())
          ((eq (member (car lst) (cdr lst)) nil)
           (cons (car lst) (rmdup (cdr lst))))
          (t (rmdup (cdr lst)))))


;;;
;;; last n els from seq
;;;
(export '(lastn))
(defun lastn (n seq)
    (nthcdr (1+ n) seq))


;;; take head length n
(export '(take))
(defun take (n seq)
    (subseq seq 0 n))


;;; take tail from n
(export '(drop))
(defun drop (n seq)
    (if (consp seq) (nthcdr n seq)
        (subseq seq n)))




;;;
;;; split seq on head tail pair
;;;
(export '(split-seq))
(defun split-seq (n seq)
    (let ((hd (take n seq))
          (tl (drop n seq)))
        (values hd tl)))



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
(defun split (src-str &key not-empty  max (delim #\Space))
    (flet ((delim-p (char) (char= char delim)))
        (reverse
         (let ((result nil)
               (start 0)
               (words 0)
               (end 0)
               (item))
             (loop
               (when (and max (>= words (1- max)))
                   (return (cons (subseq src-str start) result)))
               (setq end (position-if #'delim-p src-str :start start))
               (setq item (subseq src-str start end))
               (cond ((and not-empty (> (length item) 0))
                      (push item result)
                      (incf words))
                     ((not not-empty)
                      (push item result)
                      (incf words)))
               (unless end (return result))
               (setq start (1+ end)))))))




;;; rest from rest (i.e. without the first two)
;;; (rest2 '(1 2 3 4 5))
;;; => (3 4 5)
;;;
(export '(rest2))
(defun rest2 (form)
    (rest (rest form)))



;;;; Randoms

(export '(random))
(defun random (n)
    (#j:Math:floor (* (#j:Math:random) n)))



;;; rotate
;;; from stackoverflow
;;;
(defun rotatel (n l)
    (append (nthcdr n l) (butlast l (- (length l) n))))

(export '(rotatel))


(defun rotater (n l)
    (rotatel (- (length l) n) l))

(export '(rotater))





;;;; EOF
