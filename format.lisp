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

(defun human-readable-duration (duration &optional stream (n-parts 2))
  (check-type n-parts (integer 1 6))
  
  (multiple-value-bind (nsecs secs minutes hours days weeks)
      (local-time-duration::decode-duration duration :weeks t)
    (flet ((zero-is-nil (x)
             (if (zerop x) nil x)))
      (local-time-duration::with-designated-stream (stream stream)
        (let ((args (loop for item in (list weeks days hours minutes secs nsecs)
                          for idx upfrom 0
                          if (>= idx n-parts)
                            collect nil
                          else
                            collect (zero-is-nil item))))
          (if (every #'null args)
              (format stream "0 length")
              (apply #'format
                     stream
                     "~@[~d week~:p~]~@[ ~d day~:p~]~@[ ~d hour~:p~]~@[ ~d minute~:p~]~@[ ~d second~:p~]~@[ ~d nsec~:p~]"
                     args)))))))

(defun default-format-part (stream part-type part)
  "This is default function, define your own if you want to support other language."
  (ecase part-type
    (:weeks (format stream "~d week~:p" part))
    (:days (format stream "~d day~:p" part))
    (:hours (format stream "~d hour~:p" part))
    (:minutes (format stream "~d minute~:p" part))
    (:secs (format stream "~d second~:p" part))
    (:nsecs (format stream "~d nsec~:p" part))))

(defun human-readable-duration (duration &optional stream (n-parts 2) (format-part #'default-format-part))
  (check-type n-parts (integer 1 6))
  
  (multiple-value-bind (nsecs secs minutes hours days weeks)
      (local-time-duration::decode-duration duration :weeks t)
    (local-time-duration::with-designated-stream (stream stream)
      (let ((part-types (list :weeks :days :hours :minutes :secs :nsecs))
            (parts (list weeks days hours minutes secs nsecs)))
        (if (every #'zerop parts)
            (format stream "0 length")
            (loop with n-printed = 0
                  for part in parts
                  for part-type in part-types
                  unless (zerop part)
                    do (unless (zerop n-printed)
                         ;; Add a space between parts
                         (format stream " "))
                       (funcall format-part
                                stream
                                part-type
                                part)
                       (incf n-printed)
                  when (>= n-printed n-parts)
                    do (return)))))))

(defmethod print-object ((object duration) stream)
  (if *print-readably*
      ;; According to DECODE-DURATION, the YEAR, MONTHS and WEEKS
      ;; components are always zero.
      (multiple-value-bind (nsecs secs minutes hours days)
          (decode-duration object)
        (flet ((field (key value) (if (zerop value) () (list key value))))
          (format stream "#.~S"
                  `(duration ,@(field :day days)
                             ,@(field :hour hours)
                             ,@(field :minute minutes)
                             ,@(field :sec secs)
                             ,@(field :nsec nsecs)))))
      (print-unreadable-object (object stream :type 'duration)
        (format stream "[~d/~d/~d] ~A"
                (day-of object)
                (sec-of object)
                (nsec-of object)
                (human-readable-duration object)))))
