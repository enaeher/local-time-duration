(defsystem :cl-postgres+local-time-duration
    :description "cl-postgres integration for local-time-duration"
    :version "1.1"
    :author "WebCheckout, Inc."
    :license "MIT"
    :depends-on (:cl-postgres :local-time-duration)
    :components ((:file "cl-postgres")))
