(in-package :cl-user)

(defpackage :local-time-duration
  (:use :common-lisp
        :esrap)
  (:nicknames :ltd)
  (:export
   #:duration
   #:duration=
   #:duration/=
   #:duration>
   #:duration>=
   #:duration<
   #:duration<=
   #:duration-as
   #:duration+
   #:duration-
   #:duration/
   #:duration*
   #:duration-minimum
   #:duration-maximum
   #:human-readable-duration
   #:timestamp-difference
   #:timestamp-duration+
   #:timestamp-duration-

   #:parse-iso8601-duration))
