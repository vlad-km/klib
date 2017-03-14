;;;
;;; Chrome sql database library sketch
;;;
;;; Copyright, 2017, mvk
;;;
;;; Details see: todo.lisp
;;;
;;;

;;;
;;; sql db open
;;;
;;; (setf db (sql-dbopen "ToDo" "1.01" "Chrome ToDo lists" 200000))
;;;
(export '(sql-dbopen))
(defun sql-dbopen (name version description size)
    (let ((db (#j:window:openDatabase name version description size)))
        (if (jscl::js-null-p db)
            (setf db nil))
        db))

;;;
;;; tx.executeSql
;;;
;;; (tx-execute-sql
;;;      tx
;;;      "SELECT COUNT(*) FROM ToDo"
;;;      #()
;;;      (lambda (&rest tail) (apply #j:console:log "Ok" (flatten tail)))
;;;      (lambda (&rest tail) (apply #j:console:log "NotOk" (flatten tail))))
;;;
;;; Important
;;;      (&rest args) must be specified
;;;      Depending on the context
;;;      It can be so: (ctx result-set &rest tail)
;;;      Or so:  (ctx error)
;;;      Without this &rest, an error "too many arguments" occurs ????
;;;

(export '(sql-tx-execute))
(defun sql-tx-execute (tx sql args fn-result fn-err)
    ;;(#j:console:log "Call ExecSql Tx" sql tx)
    (funcall ((oget tx "executeSql" "bind") tx sql args fn-result fn-err))
    ;;(#j:console:log "End ExecSql TX" tx)
    )


;;;
;;; db.transaction
;;;
;;; (sql-db-transaction db
;;;      (lambda (tx)
;;;          (sql-tx-execute tx ...)
;;;           (sql-tx-execute tx ...)))
;;;
(export '(sql-db-transaction))
(defun sql-db-transaction (db fn-tx)
    (#j:console:log "Transact" "Db" db "Tx" fn-tx)
    (funcall ((oget db "transaction" "bind") db fn-tx)))


;;;
;;; install :sql *features*
;;;
;;; Another addon should check *features* and make a decision about the continuation
;;; with the issuance of diagnostics
;;;

(addon-provide :sql-feature :release 'pre-0.1)

;;;;; eof
