(in-package :cl-user)

(defpackage :local-time-duration
  (:use :common-lisp
        :cl-user)
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
   #:timestamp-difference
   #:timestamp-duration+
   #:timestamp-duration-))
