(in-package :ltd)

(defun call-with-designated-stream (stream thunk)
  (cond
    ((null stream)
     (with-output-to-string (string)
       (funcall thunk string)))
    ((eql t stream)
     (funcall thunk *standard-output*)
     *standard-output*)
    ((streamp stream)
     (funcall thunk stream)
     stream)
    (t
     (error "Unable to determine the stream designated by ~S." stream))))

(defmacro with-designated-stream ((var stream) &body body)
  "Binds VAR to the stream designated (as per cl:format's stream
argument) by the value STREAM and executes BODY.

If STREAM is nil VAR will be bound to a string-output-stream and the
resulting string will be returned, otherwise the actual stream object
used will be returned. In either case the return value of BODY is
ignored."
  `(call-with-designated-stream ,stream (lambda (,var) ,@body)))

(defun pretty-seconds (secs nsecs)
  (if (plusp nsecs)
      (format nil "~F" (float (+ secs (/ nsecs +nsecs-per-second+))))
      (format nil "~D" secs)))

(defun human-readable-duration (duration &optional stream)
  (multiple-value-bind (nsecs secs minutes hours days weeks)
      (decode-duration duration :weeks t)
    (flet ((zero-is-nil (x) (if (zerop x) nil x)))
      (with-designated-stream (stream stream)
        (if (every #'zerop (list weeks days hours minutes secs nsecs))
            (format stream "0 length")
            (format stream "~@[~d week~:p~]~@[ ~d day~:p~]~@[ ~d hour~:p~]~@[ ~d minute~:p~]~@[ ~d second~:p~]~@[ ~d nsec~:p~]"
                    (zero-is-nil weeks)
                    (zero-is-nil days)
                    (zero-is-nil hours)
                    (zero-is-nil minutes)
                    (zero-is-nil secs)
                    (zero-is-nil nsecs)))))))

(defmethod print-object ((object duration) stream)
  (print-unreadable-object (object stream :type 'duration)
    (format stream "[~d/~d/~d] ~A"
            (day-of object)
            (sec-of object)
            (nsec-of object)
            (human-readable-duration object))))
