(in-package :local-time-duration)

(export 'set-local-time-duration-cl-postgres-reader :local-time-duration)

(defun set-local-time-duration-cl-postgres-reader (&optional (table cl-postgres:*sql-readtable*))
  (cl-postgres::set-interval-reader (lambda (months days usec)
                                      (assert (= 0 months)
                                              (months)
                                              "local-time-duration does not yet support intervals with months.")
                                      (duration :day days :nsec (* usec 1000)))
                                    table))

(defmethod cl-postgres:to-sql-string ((duration duration))
  (format-iso8601-duration nil duration))
