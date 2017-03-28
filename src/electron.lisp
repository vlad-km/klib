
;;; Electron API



;;;(register-global-shortcut (lisp-to-js "CommandOrControl+Alt+K")
;;;                          (lambda ()
;;;                              (dialog-show-message-box *short-cut)))


;;;(register-global-shortcut (lisp-to-js "CommandOrControl+Alt+L")
;;;                          (lambda ()
;;;                              (format t "<font> color='red'><h2>Hello</h2></font>")
;;;                              (dialog-show-message-box *short-cut)))




(defparameter *electron (require "electron"))
(defparameter *os (require "os"))
(defparameter *App nil)
(defparameter *Dialog nil)
(defparameter *globalShortcut nil)
(defparameter *remote nil)



(setf *remote (oget *electron "remote"))
(setf *App (oget *electron "remote" "app"))
(setf *Dialog (oget *electron "remote" "dialog"))
(setf *globalShortcut (oget *electron "remote" "globalShortcut"))

(setf #j:electron (#j:require "electron"))
(setf #j:remote #j:electron:remote)
(setf #j:mApp #j:electron:remote:app)
(setf #j:dialog #j:electron:remote:dialog)
(setf #j:mGlobalShortcut #j:electron:remote:globalShortcut)
(setf #j:mShell #j:electron:shell)
                                        ;(setf #j:BrowserWindow #j:electron:remote:BrowserWindow)
(setf #j:mPath (#j:require "path"))
(setf #j:mOs (#j:require "os"))



(fset 'register-global-shortcut #j:mGlobalShortcut:register)
(fset 'dialog-show-message-box #j:electron:remote:dialog:showMessageBox)
(fset 'dialog-show-open-dialog #j:electron:remote:dialog:showOpenDialog)

(fset 'os-home-dir #j:mOs:homedir)
(fset 'shell-show-item-folder #j:mShell:showItemInFolder)
(fset 'shell-open-external #j:mShell:openExternal)

(fset 'path-join #j:mPath:join)


(defun make-browser-window (opt)
    (make-new #j:electron:remote:BrowserWindow opt) )

(defun win-load-url (win url)
    (funcall ((oget win "loadURL" "bind") win url)))

(defun win-show (win)
    (funcall ((oget win "show" "bind") win)))



;;;const modalPath = path.join('file://', __dirname, '../../sections/windows/modal.html')
;;;  let win = new BrowserWindow({ width: 400, height: 320 })
;;;  win.on('close', function () { win = null })
;;;  win.loadURL(modalPath)
;;;  win.show()




(defun to-array (&rest args)
    (list-to-vector args))


(defparameter *op-dir
  (make-array 2 :initial-element (lisp-to-js "openFile")))
(setf (aref *op-dir 1) (lisp-to-js "openDirectory"))




(defun show-files (files)
    (format t "Принято: ~a~%" (length files))
    (dotimes (i (length files))
        (format t "~a~%" (aref files i))))


;;;(dialog-show-open-dialog
;;; (make-js-object "properties" *op-dir)
;;; (lambda (files)
;;;     (format t "Files ~a~%" files)
;;;     (show-files files) ))



(defparameter *button-ok (make-array 1 :initial-element (lisp-to-js "OK")))

(defparameter *short-cut
  (make-js-object "type" "info"
                  "message" "Success"
                  "detail" "You pressed the keybinding"
                  "buttons" *button-ok))










;;; EOF
