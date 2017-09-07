;;; Klib
;;; DOM manipulation functions
;;;
;;;
;;; Copyright (C) 2017 mvk (github.com/vlad-km)
;;;
;;;


;;;
;;; Depends on: string.lisp
;;;             attic.lisp
;;;
;;;


(export '( dom-get-body
          dom-get-head
          dom-get-title
          dom-set-title
          dom-get-element-value
          dom-set-element-value
          dom-get-element-by-id
          dom-set-attribute
          dom-get-attribute
          dom-remove-attribute
          dom-set-event
          dom-element-p
          dom-escape-html
          dom-get-inner-html
          dom-set-inner-html
          dom-create-text-node
          dom-replace-text-node
          dom-remove
          dom-add-class
          dom-remove-class
          dom-have-class-p
          dom-append
          dom-mount
          dom-clear
          dom-create
          dom-create-input
          dom-create-text
          dom-create-button
          dom-create-checkbox
          dom-parent-node
          dom-element-node-name
          dom-element-last-child-p
          dom-element-child-count
          dom-element-childs-collection
          dom-element-first-child
          dom-element-last-child
          dom-element-next-sibling
          dom-element-parent-node
          dom-insert-before
          dom-insert-after
          dom-insert-top
          dom-insert ))


;;; window.document.body
;;;

;;;
(export '(*dom-body*))
(defparameter *dom-body* #j:window:document:body)

(defun dom-get-body ()
    #j:window:document:body)

;;;
;;; window.document.head
;;; see above
;;;
(export '(*dom-head*))
(defparameter *dom-head* #j:window:document:head)

(defun dom-get-head ()
    #j:window:document:head)


;;; window.document.title
;;;
(defun dom-get-title ()
    (oget #j:window:document "title"))

(defun dom-set-title (text)
    (setf (oget #j:window:document "title") text))


;;; element.value
;;;
;;; todo: test
;;;
(defun dom-get-element-value (element)
    (oget element "value"))

(defun dom-set-element-value (text)
    (setf (oget element "value") text) )

;;; window.document.getElementById
;;;
;;; Error condition if div with id does not exist
;;; ERROR: Cannot use 'in' operator to search for 'multiple-value' in null
;;;
;;; Error handling, if need:
;;;
;;;    (handler-case (dom-get-element-by-id id) (error (err) nil))
;;;
;;; 100 id's => 0.004 sec
;;;
(defun dom-get-element-by-id (id)
    (#j:window:document:getElementById id))


;;; window.document.getElementsByClassName
;;;
;;; todo: Need test
;;;
(defun dom-get-elements-by-class (id)
    (#j:window:document:getElementsByClassName id))



;;; elem.hasAttribute(name)
;;;
;;; (dom-has-attribute *div "Order")
;;; => t, nil
;;;
(export '(dom-has-attribute))
(defun dom-has-attribute (elem name)
    (funcall ((oget elem "hasAttribute" "bind") elem name)))


;;; elm.setAttribute(val)
;;;
;;; todo: Need test
;;;
(defun dom-set-attribute (elm attr val)
    (funcall ((oget elm "setAttribute" "bind")  elm attr val)))

;;; elm.getAttribute()
;;;
;;; todo: Need test
;;;
(defun dom-get-attribute (elm attr)
    (let ((rc (funcall ((oget elm "getAttribute" "bind") elm attr))))
        (if (js-null-p rc)
            nil
            rc)))


;;; elm.removeAttribute(attr)
;;;
;;; todo: Need test
;;;
(defun dom-remove-attribute (elm attr)
    (funcall ((oget elm "removeAttribute" "bind") elm attr)))

;;; elm.onchange = function
;;; elm.onclick = function
;;;
;;; (defun help-me (event) (format t "Help text~%"))
;;; (dom-set-event btn-help "onclick" #'help-me)
;;;
(defun dom-set-event (elm event function)
    (setf (oget elm event) function))

;;;
;;; (dom-element-p "sss") => nil
;;; (dom-element-p (dom-create "div")) => t
;;;
;;; todo: Need test
;;; null or js-null
;;;
(defun dom-element-p (u)
    (when (objectp u)
        (if (not (null (oget u "appendChild"))) t nil)))


;;; create div with escaped html text
;;;
;;; (dom-escape-html "<p>text</p>")
;;; => [object HTMLDivElement]
;;;
(defun dom-escape-html (str)
    (let* ((div (#j:window:document:createElement "div"))
           (text (#j:window:document:createTextNode str)))
        (funcall ((oget div "appendChild" "bind") div text))
        div ))

;;; dom-get-inner-html
;;;
;;; (dom-get-inner-html (dom-escape-html "<p>text</p>"))
;;; => "&lt;p&gt;test&lt;/p&gt;"
;;;
;;;
(defun dom-get-inner-html (div)
    (oget div "innerHTML"))


;;; dom-set-inner-html
;;;
;;; (setq *es (dom-escape-html "<p>text</p>"))
;;; (string (dom-set-inner-html *es "One two three"))
;;; => "One two three"
;;;
(defun dom-set-inner-html (div str)
    (setf (oget div "innerHTML") str))


;;; dom-create-text-node
;;;
;;; var el = window.document.createTextNode("text")
;;;
;;; => [object HTMLDivElement]
;;;
;;; note: 1) Raise error condition with message "Out of range."
;;;       2) Do not use
;;;          If need a text element, then appendChild will do fine.
;;;             (dom-append (aref *out9 4)
;;;                         (dom-append (dom-create "div") "Teeeetttt"))
;;;
;;;          Important:
;;;          If the dom element has such descendants
;;;          Jscl will not work with this list ("Out of range." Error)
;;;          07.03.2016
;;;

;;; DO NOT USE ????
;;; todo: test and workaround
(defun dom-create-text-node (str)
    (#j:window:document:createTextNode str))


;;; dom-replace-text-node
;;;
;;; Note: Its Kludge for TextNode
;;;
(defun dom-replace-text-node (div str)
    (setf (oget div "innerHTML") str))


;;; dom-remove
;;;
;;; (dom-remove (dom-create "div"))
;;;
;;; Time 0.02 ms
;;; todo: performance
;;;        done: 0.002s on 100 div's
;;;
(defun dom-remove (elem)
    (if (not (js-null-p (oget elem "parentNode")))
        (let* ((var (oget elem "parentNode")))
            (funcall ((oget var "removeChild" "bind") var elem )))))


;;; dom-class-name
;;;
;;; (dom-class-name div)
;;; => string "class1 class2 ...classn"
;;;    i.e. el.className
;;;

(export '(dom-class-name))
(defun dom-class-name (elem)
    (oget elem "className"))


;;; dom-get-class-names
;;;
;;; (dom-get-class-names elem)
;;; =>
;;;   (list "name1" "name2") or nil
;;;
;;; (dom-get-class-names elem :to 'vector)
;;; =>
;;;   #("name1" "name2")
;;;
;;; => nil
;;;    if elem hasnt any class
;;;

(export '(dom-get-class-name))
(defun dom-get-class-name (elem &key (to 'list))
    (let ((cls (lisp-to-js (oget elem "className")))
          (cn nil))
        (if (> (oget cls "length") 0)
            (map to (lambda (x) (js-to-lisp x))
                 (funcall ((oget cls "split" "bind") cls " "))))))



;;; dom-contains-class
;;;
;;; (dom-contains-class class-name)
;;; => logical  i.e. exists/none
;;;

(export '(dom-contains-class))

(defun dom-contains-class (elem class-name)
    (let* ((collection (oget elem "classList")))
        (funcall ((oget collection "contains" "bind") collection class-name))))

;;; dom-add-class
;;;
;;; (dom-add-class div "invisible")
;;;

;;;
;;; collection method Add return "undefined"!!
;;;
(export '(dom-add-class))

(defun dom-add-class (elem class)
    (let* ((collection (oget elem "classList")))
        (funcall ((oget collection "add" "bind") collection class))
        (values-list nil)))


;;; dom-remove-class
;;;
;;; (dom-remove-class div "visible")
;;;
(export '(dom-remove-class))
(defun dom-remove-class (elem class)
    (let* ((collection (oget elem "classList")))
        (funcall ((oget collection "remove" "bind") collection class))
        (values-list nil)))


;;; dom-toggle-class
;;;
;;; (dom-toggle-class div "visible")
;;; =>
;;;   if class exists - remove it
;;;   else add new class to dom element
;;;
(export '(dom-toggle-class))
(defun dom-toggle-class (elem class)
    (let* ((collection (oget elem "classList")))
        (funcall ((oget collection "toggle" "bind") collection class))
        (values-list nil)))


;;; dom-have-class-p
;;;
;;; (dom-have-class-p div "invisible") => t
;;;

#|
(defun dom-have-class-p (elem cls)
    (if (find cls (split (oget elem "className") :not-empty t) :test #'equal)
        t
        nil))
|#

(fset 'dom-have-class-p #'dom-contains-class)




;;; dom-append
;;;
;;; parent.appendChild(children)
;;;
;;; now return parent with new childs
;;;
(defun dom-append (parent &rest children)
    (mapc
     (lambda (e)
         (cond ((dom-element-p e)
                (funcall ((oget parent "appendChild" "bind" ) parent e)))
               (t
                (dom-set-inner-html  parent (concat (dom-get-inner-html parent) e)))))
     children)
    parent)

;;;
;;; dom-mount
;;;
;;; parent.appendChild(children)
;;;
(defun dom-mount (parent &rest children)
    (if (not (dom-element-p parent))
        (error "dom-mount: parent must be dom. ~s" parent))
    (mapc
     (lambda (e)
         (cond ((dom-element-p e)
                (funcall ((oget parent "appendChild" "bind" ) parent e)))
               (t
                (dom-set-inner-html  parent (concat (dom-get-inner-html parent) e)))))
     children)
    parent)


;;; dom-element-clear
;;;
;;; elm.innerHTML = ""
;;;

(export '(dom-element-clear))
(defun dom-element-clear (elm)
    (setf (oget elm "innerHTML") ""))


;;; dom-create
;;;
;;; Creates an dom element 'tagname'  with the specified properties and callbacks
;;;
;;;   (defparameter div-control
;;;           (dom-create "div" (pair '("id" "class")
;;;                                   '("control-id" "control default-font"))))
;;;
;;;   (defparameter div-br (dom-create "br"))
;;;
;;;   (defparameter btn-run
;;;             (dom-create-button "Run"
;;;                   (pair '("id" "class")
;;;                         '("run-button-id" "button-default"))))
;;;
;;;   (defparameter checkbox
;;;              (dom-create-checkbox
;;;                        (list (cons "id" "unit-id")
;;;                              (cons "value" "unit-value")) ))
;;;
;;;   (defparameter checkbox-title
;;;            (dom-create "label" (list (cons "for" "unit-id") )) ) )
;;;
;;;   (dom-mount checkbox-title checkbox-el (dom-create-text-node unit-name))
;;;   (dom-mount (dom-get-body) checkbox-title div-br div-control)
;;;  =>
;;;       Error: Out a range  (see comments for dom-create-text-node)
;;;   workaround
;;;
;;;   (dom-mount (dom-get-bode)
;;;         (dom-append checkboc-title unit-name)
;;;         div-br
;;;         div-control)
;;;
;;;
(defun dom-create (tagname &optional props callbacks)
    (let ((obj (#j:window:document:createElement tagname)))
        (if props
            (mapc (lambda (x) (dom-set-attribute obj (car x) (cdr x))) props))
        (if callbacks
            (mapc (lambda (x) (dom-set-event obj (car x) (cdr x))) callbacks))
        obj))

;;;
;;; dom-create-input
;;;
;;; <input>
;;;
(defun dom-create-input (&optional props callbacks)
    (dom-create "input" props callbacks))

;;; dom-create-text
;;;
;;; <input text>
;;;
(defun dom-create-text (&optional props callbacks)
    (dom-create-input (append props '(("type" . "text"))) callbacks))

;;; dom-create-button
;;;
;;; (dom-create-button "Help")
;;;    => [object HTMLInputElement]
;;;    => <input type="button" value="Help">
;;;
;;; (setq *bhm (dom-create-button "Help" (pair '("id" "class") '("help-id" "help-class"))
;;;                                      (list (cons "onclick" #'help-me) )))
;;;   => [object HTMLInputElement]
;;;   => <input id="help-id" class="help-class" type="button" value="Help">
;;;
;;;
(defun dom-create-button (text &optional props callbacks)
    (dom-create-input (append props (list '("type" . "button") (cons "value" text))) callbacks))


;;; dom-create-checkbox
;;;
;;; (dom-create-checkbox (pair '("id"          "class"       "value")
;;;                            '("check-11-id" "check-class" "22"))
;;;                      (pair '("onchange") (list #'check-selector)))
;;;
;;;   => [object HTMLInputElement]
;;;   => <input type="checkbox" id="check-11-id" class="check-class" value="22">
;;;
(defun dom-create-checkbox (&optional props callbacks)
    (dom-create-input (cons '("type" . "checkbox") props) callbacks))



;;; dom-parent-node
;;;
;;; element.parentNode
;;;
;;;    (dom-parent-node *chk)
;;;     => [object HTMLDivElement]
;;;
;;; Right form:
;;;
;;;    (if (not (js-null-p (dom-parent-node div)))
;;;           .... div has parent
;;;           .... div hasnt parent)
;;;
;;;
(defun dom-parent-node (element)
    (oget element "parentNode"))


;;; dom-element-node-name
;;;
;;; element.nodeName
;;;
;;; (dom-element-node-name *chk)
;;; => "INPUT"
;;;
(defun dom-element-node-name (element)
    (oget element "nodeName"))


;;; dom-element-has-child-p
;;;
;;; element.hashChildNodes
;;;
;;; (dom-element-hash-child-p *chk)
;;;   => t or nil
;;;
(defun dom-element-has-child-p (element)
    (funcall ((oget element "hasChildNodes" "bind") element)))


;;; dom-element-child-count
;;;
;;; (dom-child-element-count pid)
;;; => 0 or collection size
;;;
(defun dom-element-child-count (element)
    (oget element "childElementCount"))


;;; dom-element-childs-collection
;;;
;;; Get dom element childs collection
;;;
;;; Return nil, if element hasnt children
;;; or array of childrens elements
;;;
;;; Note: JSCL dont properly processing dynamical array from js
;;;       07.03.2017  Its wrong
;;;
;;;  The reason in the textNode (there is such a tricky element)
;;;
;;; Next case
;;;
;;;       div
;;;         |
;;;         span-1
;;;         |
;;;         span-2
;;;         |
;;;         text
;;;         |
;;;         span-3
;;;
;;;       Its wrong dom-structure
;;;
;;;       (dom-element-child-collection div)
;;;        => Error: Out of range.
;;;
;;;       div
;;;         |
;;;         span-1
;;;         |
;;;         span-2
;;;         |
;;;         div
;;;         | |
;;;         | text
;;;         |
;;;         span-3
;;;
;;;        Its correct
;;;
;;;        (dom-element-childs-collection div)
;;;        => #(#<JS-OBJECT [object HTMLSpanElement]>
;;;             #<JS-OBJECT [object HTMLSpanElement]>
;;;             #<JS-OBJECT [objectHTMLDivElement]>
;;;             #<JS-OBJECT [object HTMLSpanElement]>)
;;;
;;;


;;;
;;;  It causes an error if the collection has a nodeText elements
;;;  => ERROR: Out of range.
;;;

(defun dom-element-childs-collection (element)
    (oget element "childNodes"))



;;; todo: node.children


;;; dom-element-first-child
;;;
;;;  (dom-element-first-child pid)
;;;  => [object HTMLDivElement]
;;;
;;; Note: ERROR Condition if pid hasnt children
;;;
;;; todo: need test
(defun dom-element-first-child (element)
    (if (js-null-p (oget element "firstElementChild"))
        nil
        (oget element "firstElementChild")))

;;; dom-element-last-child
;;;
;;;  (dom-element-last-child pid)
;;;  => [object HTMLDivElement]
;;;
;;; Note: ERROR Condition if pid hasnt children
;;;
;;; todo: need test
(defun dom-element-last-child (element)
    (if (js-null-p (oget element "lastElementChild"))
        nil
        (oget element "lastElementChild")))




;;; dom-element-next-sibling
;;;
;;; NOte: ERROR condition if hasnt sibling
;;; todo: need test
(defun dom-element-next-sibling (element)
    (if (js-null-p (oget element "nextSibling"))
        nil
        (oget element "nextSibling")
        ))


;;; dom-element-parent-node
;;;
(defun dom-element-parent-node (element)
    (oget element "parentNode"))



;;; dom-insert-before
;;;
(defun dom-insert-before (element new-element)
    (funcall ((oget element "parentNode" "insertBefore" "bind")
              (oget element "parentNode")
              new-element
              element)))

;;; dom-insert-after
;;;
;;; inserts a new element after the specified element
;;;
;;; Note: ERROR condition hasnt right sibling
;;;
;;; Workaround:
;;;     If you dont check sibling (dom-element-next-sibling) => nil or [dom element]
;;;     its you problem, catch error (for example: (handler-case (...) (error (msg))) )
;;;
(defun dom-insert-after (element new-element)
    (funcall ((oget element "parentNode" "insertBefore" "bind")
              (oget element "parentNode")
              new-element
              (oget element "nextSibling") )))


;;; dom-insert-top
;;;
;;; insert new element to top dom stucture
;;;
;;; Note: ERROR condition if element hasnt child
(defun dom-insert-top (element new-element)
    (funcall ((oget element "insertBefore" "bind")
              element
              new-element
              (oget element "firstElementChild"))))


;;; dom-insert
;;;
;;; insert new element to bottom dom stucture
;;; is the same thing that (dom-append root div)
;;; or (dom-mount root div)
;;;
(defun dom-insert (element  new-element)
    (funcall ((oget element "appendChild" "bind")
              element
              new-element)))


;;;
;;; install :dom *features*
;;;

(addon-provide :dom)


;;; EOF
