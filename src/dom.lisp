;;; DOM manipulation functions
;;;
;;;
;;; Yes, this is not the jquery-way
;;; I know about it
;;;
;;; Copyleft, 2016 mvk
;;;
;;; Release: Pre-0.1
;;; Version: Alpha-1.0
;;;
;;;
;;; Depends on:
;;;
;;;
;;; Tested: Chrome
;;;         Win 7/ ccl v1.11-64
;;;         JSCl master branch
;;;             https://github.com/davazp/jscl/commit/ac132c0e128debe265f9a3fe74e1a96f78fea8cb
;;;
;;; Note:
;;;     1) Some functions raise error condition because current JSCL
;;;        was not correct processing null from JS
;;;     2) Some functions may raised error condition by cause
;;;        returned dynamical array data type from JS.
;;;               note:  07.03.2017 - its wrong.
;;;                      See dom-childs-collection comments
;;;;    3) The above two points are valid for (Nodes:
;;;        childs,parent,sibling node and nodes collections).
;;;        It requires additional testing
;;;     4) Host (win7/ccl-1.11) compiled to dom.js
;;;        for run-time compiled on browser, before, need export
;;;        oset, oget, concat, join from jscl package, as
;;;        external functiions (i.e. jscl::oget etc., etc.)
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
(defun dom-get-body ()
    #j:window:document:body)


;;; window.document.head
;;;
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
(defun dom-get-element-value (element)
    (oget element "value"))

(defun dom-set-element-value (text)
    (setf (oget element "value") text) )

;;; window.document.getElementById
;;;
(defun dom-get-element-by-id (id)
    (#j:window:document:getElementById id))

;;; elm.setAttribute(val)
;;;
(defun dom-set-attribute (elm attr val)
    (funcall ((oget elm "setAttribute" "bind")  elm attr val)))

;;; elm.getAttribute()
;;;
(defun dom-get-attribute (elm attr)
    (funcall ((oget elm "getAttribute" "bind") elm attr)))

;;; elm.removeAttribute(attr)
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

;;; DO NOT USE
(defun dom-create-text-node (str)
    (#j:window:document:createTextNode str))


;;; dom-replace-text-node
;;;
(defun dom-replace-text-node (div str)
    (setf (oget div "innerHTML") str))


;;; dom-remove
;;;
;;; (dom-remove (dom-create "div"))
;;;
;;; Time 0.02 ms
;;; todo: performance
(defun dom-remove (elem)
    (if (not (js-null-p (oget elem "parentNode")))
        (let* ((var (oget elem "parentNode")))
            (funcall ((oget var "removeChild" "bind") var elem )))))

;;; dom-add-class
;;;
;;; (dom-add-class div "invisible")
;;; if css definition ".invisible" exist
;;;
(defun dom-add-class (elem cls)
    (symbol-macrolet ((class-name (oget elem "className")))
        (let ((old-cls (or class-name "")))
            (setf class-name
                  (if old-cls
                      (concat old-cls " " cls)
                      cls)))))

;;; dom-remove-class
;;;
;;; (dom-remove-class div "visible")
;;; if css definition ".invisible" exist
;;;
(defun dom-remove-class (elem cls)
    (symbol-macrolet ((class-name (oget elem "className")))
        (let ((old-classes (split-str '(#\space) class-name)))
            (setf class-name (join (remove cls old-classes :test #'equal) " ")))))


;;; dom-have-class-p
;;;
;;; (dom-have-class-p div "invisible") => t
;;;
(defun dom-have-class-p (elem cls)
    (let ((classes (split-str '(#\space) (oget elem "className"))))
        (if (find cls classes :test #'equal)
            t
            nil)))

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
;;; Synonim for dom-append
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


;;; dom-clear
;;;
;;; elm.innerHTML = ""
;;;
(defun dom-clear (elm)
    (setf (oget elm "innerHTML") ""))


;;; dom-create
;;;
;;; Creates an dom element 'tagname'  with the specified properties and callbacks
;;;
;;;   (defparameter div-control
;;;           (dom-create "div" (pair '("id" "class")
;;;
;;;   (defparameter div-br (dom-create "br"))
;;;                                  '("control-id" "control default-font"))))
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
;;;
;;;
;;;
(defun dom-create (tagname &optional props callbacks)
    (let ((obj (#j:window:document:createElement tagname)))
        (if props
            (mapc (lambda (x) (dom-set-attribute obj (car x) (cdr x))) props))
        (if callbacks
            (mapc (lambda (x) (dom-set-event obj (car x) (cdr x))) callbacks))
        obj))

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


#|

(defun dom-element-childs-collection (element)
    (let ((count (oget element "childElementCount"))
          (res))

        (cond ((> count 0)
               (do ((i 0  (1+ i)))
                   ((= i count) i)
                   (push (oget element "childNodes" (string (1+ i))) res))
               (reverse res))
              (t res))))
|#

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
(defun dom-element-first-child (element)
    (oget element "firstElementChild"))


;;; dom-element-last-child
;;;
;;;  (dom-element-last-child pid)
;;;  => [object HTMLDivElement]
;;;
;;; Note: ERROR Condition if pid hasnt children
;;;
(defun dom-element-last-child (element)
    (oget element "lastElementChild"))



;;; dom-element-next-sibling
;;;
;;; NOte: ERROR condition if hasnt sibling
(defun dom-element-next-sibling (element)
    (oget element "nextSibling"))


;;; dom-element-parent-node
;;;
(defun dom-element-parent-node (element)
    (oget element "parentNode"))



;;; dom-insert-before
;;;
(defun dom-insert-before (element new-element)
    (funcall ((oget element "parentNode" "insertBefore" "bind")
              (dom-element-parent-node element)
              new-element
              element)))

;;; dom-insert-after
;;;
;;; inserts a new element after the specified element
;;;
;;; Note: ERROR condition
(defun dom-insert-after (element new-element)
    (funcall ((oget element "parentNode" "insertBefore" "bind")
              (dom-element-parent-node element)
              new-element
              (dom-element-next-sibling element) )))


;;; dom-insert-top
;;;
;;; insert new element to top dom stucture
;;;
;;; Note: ERROR condition if element hasnt child
(defun dom-insert-top (element new-elemen)
    (funcall ((oget element "insertBefore" "bind")
              element
              new-element
              (dom-element-first-child element))))


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


;;; EOF
