(defpackage :teno.db.rdb.mysql
  (:use :cl)
  (:export :make-locator
           :mysql
           :create-database))
(in-package :teno.db.rdb.mysql)

(defstruct locator user host port)

(progn
  (remhash :timestamp cl-mysql:*type-map*)
  (remhash :date      cl-mysql:*type-map*)
  (remhash :time      cl-mysql:*type-map*)
  (remhash :datetime  cl-mysql:*type-map*)
  (remhash :newdate   cl-mysql:*type-map*))

(defclass mysql (teno.db:db)
  ((db-name
    :initarg :db-name
    :reader mysql-db-name)
   (locator
    :initarg :locator
    :reader mysql-locator)))

(defclass connection (teno.db.rdb:connection)
  ((impl
    :initarg :impl
    :reader connection-impl)))

(defmethod teno.db:call-with-connection ((db mysql) fn)
  (let ((db-name (mysql-db-name db))
        (locator (mysql-locator db)))
    (let ((conn-impl (cl-dbi:connect
                      :mysql
                      :database-name db-name
                      :username (locator-user locator)
                      :host (locator-host locator)
                      :port (locator-port locator))))
      (unwind-protect
           (funcall fn (make-instance 'connection :impl conn-impl))
        (cl-dbi:disconnect conn-impl)))))

(defun execute (conn query args)
  (let ((q (cl-dbi:prepare (connection-impl conn)  query)))
    (dbi:fetch-all (dbi:execute q args))))

(defun to-mysql-timestamp-string (timestamp)
  (local-time:format-timestring nil timestamp
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\space
             (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:nsec 9))))

(defun parse-mysql-timestamp-string (string)
  (local-time:parse-timestring string :date-time-separator #\Space))

(defmethod teno.db.rdb:timestamp-to-string ((conn connection)
                                            (ts local-time:timestamp))
  (to-mysql-timestamp-string ts))

(defmethod teno.db.rdb:parse-timestamp ((conn connection) (s string))
  (parse-mysql-timestamp-string s))

(defmethod teno.db.rdb:insert-into ((conn connection)
                                    table-name
                                    column-name-list
                                    values-list)
  (execute conn
   (with-output-to-string (s)
     (format s "INSERT INTO ~A" table-name)
     (format s " (~{~A~^,~})" column-name-list)
     (format s " VALUES ~{~A~^,~}"
             (let ((column-count (length column-name-list)))
               (loop repeat (length values-list)
                     for vals = (make-list column-count
                                           :initial-element "?")
                     collect (format nil "(~{~A~^,~})" vals)))))
   (reduce #'append values-list)))

(defun parse-clause (clause)
  (labels ((rec (clause k)
             (if (not (keywordp (car clause)))
                 (funcall k
                  (if (null clause)
                      ""
                      (format nil "~A" clause))
                  nil)
                 (ecase (car clause)
                   ((:and :or)
                    (let ((op (car clause)))
                      (destructuring-bind (left right) (cdr clause)
                        (rec left
                         (lambda (l-cond l-acc-values)
                           (rec right
                            (lambda (r-cond r-acc-values)
                              (funcall k
                               (format nil "~A ~A ~A" l-cond op r-cond)
                               (append l-acc-values r-acc-values)))))))))
                   ((:fn)
                    (let ((name (second clause))
                          (arg (third clause)))
                      (rec arg
                        (lambda (cond acc-values)
                          (funcall k
                            (format nil "~A(~A)" name cond)
                            acc-values)))))
                   ((:in :=)
                    (let ((op (car clause))
                          (column-name (second clause)))
                      (rec (third clause)
                       (lambda (cond acc-values)
                         (funcall k
                          (format nil "(~A ~A ~A)" column-name op cond)
                          acc-values)))))
                   (:p
                    (let ((values (alexandria:ensure-list
                                   (second clause))))
                      (funcall k
                       (format nil "(~{~A~^,~})"
                        (make-list (length values) :initial-element "?"))
                       values)))
                   (:where
                    (rec (second clause)
                     (lambda (cond acc-values)
                       (funcall k
                        (format nil "WHERE ~A" cond)
                        acc-values))))))))
    (rec clause #'list)))

(defmethod teno.db.rdb:delete-from ((conn connection)
                                    table-name
                                    &key where)
  (let ((args nil))
    (let ((query-string
           (with-output-to-string (s)
             (format s "DELETE FROM ~A" table-name)
             (when where
               (destructuring-bind (cond-string values)
                   (parse-clause (list :where where))
                 (format s " ~A" cond-string)
                 (alexandria:appendf args values))))))
      (execute conn query-string args))))

(defun convert-select-query (column-names table-name where order-by)
  (let ((args nil))
    (let ((query-string
           (with-output-to-string (s)
             (format s "SELECT ~A FROM ~A" column-names table-name)
             (when where
               (destructuring-bind (cond-string vals)
                   (parse-clause (list :where where))
                 (format s " ~A" cond-string)
                 (alexandria:appendf args vals)))
             (when order-by
               (format s " ORDER BY ~A" order-by)))))
      (list query-string args))))

(defmethod teno.db.rdb:select-from ((conn connection)
                                    column-names
                                    table-name
                                    &key where
                                         order-by)
  (destructuring-bind (query-string args)
      (convert-select-query column-names table-name where order-by)
    (let ((plists (execute conn query-string args)))
      (mapcar (lambda (plist)
                (mapcar #'cdr (alexandria:plist-alist plist)))
              plists))))

;;;

(defun create-database (mysql-dir db-name locator)
  (teno.db:with-connection (conn (make-instance 'mysql
                                  :db-name nil
                                  :locator locator))
    (cl-dbi:do-sql (connection-impl conn)
      (format nil "CREATE DATABASE IF NOT EXISTS ~A" db-name)))
  (teno.db:with-connection (conn (make-instance 'mysql
                                  :db-name db-name
                                  :locator locator))
    (dolist (sql (cl-ppcre:split
                  (format nil "~%~%")
                  (alexandria:read-file-into-string
                   (merge-pathnames mysql-dir "./teno-ddl.sql"))))
      (cl-dbi:do-sql (connection-impl conn) sql))))

;;;
               
(defmethod teno.db.rdb:memo-select ((conn connection))
  (mapcar
   (alexandria:compose
    (alexandria:curry #'teno.db.rdb::parse-memo conn)
    (lambda (plist)
      (mapcar #'cdr (alexandria:plist-alist plist))))
   (execute conn "SELECT memo_id, created_on FROM memos LIMIT 0, 50" nil)))

(defmethod teno.db.rdb:memo-text-update ((conn connection)
                                         (memo-id teno.id:id)
                                         (text teno.memo:text))
  (execute
   conn
   "UPDATE memo_text SET type = ?, string = ? WHERE memo_id = ?"
   (list (symbol-name (teno.memo:text-type text))
         (teno.memo:text-string text)
         (teno.id:to-string memo-id))))

(defmethod teno.db.rdb:memo-text-string-select
    ((conn connection)
     (memo-id-list list))
  (let ((rows (teno.db.rdb:select-from
               conn
               "memo_id, string"
               "memo_text"
               :where `(:in "memo_id" (:p ,(mapcar #'teno.id:to-string
                                                   memo-id-list))))))
    (dolist (r rows)
      (setf (second r) (if (second r)
                           (babel:octets-to-string (second r))
                           "")))
    rows))
