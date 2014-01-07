(defpackage :local-time-duration-system (:use #:asdf #:cl))
(in-package :local-time-duration-system)

(defsystem :local-time-duration
    :description "local-time-duration: Simple duration functionality on top of local-time"
    :version "1.0"
    :author "WebCheckout, Inc."
    :license "MIT"
    :depends-on (:local-time :alexandria)
    :serial t
    :components
    ((:file "package")
     (:file "defcomparator")
     (:file "duration")
     (:file "timestamp")))
