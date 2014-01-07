(defpackage :local-time-duration-system (:use #:asdf #:cl))
(in-package :local-time-duration-system)

(defsystem :local-time-duration
    :depends-on (:local-time :alexandria)
    :serial t
    :components
    ((:file "package")
     (:file "defcomparator")
     (:file "duration")
     (:file "timestamp")))
