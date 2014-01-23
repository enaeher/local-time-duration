(defsystem :cl-postgres+local-time-duration
  :depends-on (:cl-postgres :local-time-duration)
  :components ((:file "cl-postgres")))
