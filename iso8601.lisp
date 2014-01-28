(in-package :ltd)

(defrule digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  (:text t))

(defrule integer (+ digit)
  (:text t))

(defrule iso8601-quantity/whole
    integer
  (:function
    (lambda (string)
      (parse-integer string :radix 10))))

(defrule iso8601-quantity/fractional
    (and integer (or #\, #\.) integer)
  (:destructure (whole _ fraction)
    (declare (ignore _))
    (+ (parse-integer whole :radix 10)
       (/ (parse-integer fraction :radix 10)
          (expt 10 (length fraction))))))

(defrule iso8601-quantity
    (or iso8601-quantity/fractional
        iso8601-quantity/whole))

(defrule iso8601-year (and iso8601-quantity #\Y)
  (:lambda (production) (list :year (first production))))

(defrule iso8601-month (and iso8601-quantity #\M)
  (:lambda (production) (list :month (first production))))

(defrule iso8601-day (and iso8601-quantity #\D)
  (:lambda (production) (list :day (first production))))

(defrule iso8601-hour (and iso8601-quantity #\H)
  (:lambda (production) (list :hour (first production))))

(defrule iso8601-minute (and iso8601-quantity #\M)
  (:lambda (production) (list :minute (first production))))

(defrule iso8601-second (and iso8601-quantity #\S)
  (:lambda (production) (list :second (first production))))

(defrule iso8601-duration-date
    (and (? iso8601-year)
         (? iso8601-month)
         (? iso8601-day))
  (:lambda (production)
    (apply #'append production)))

(defrule iso8601-duration-time
    (and #\T
         (? iso8601-hour)
         (? iso8601-minute)
         (? iso8601-second))
  (:lambda (production)
    (apply #'append (cdr production))))

(defun make-duration-from-time-values (values)
  (let ((month 0)
        (day 0)
        (hour 0)
        (minute 0)
        (second 0))
    (loop for (key value) on values by #'cddr
          do (ecase key
               (:year (incf month (* 12 value)))
               (:month (incf month value))
               (:week (incf day (* value 7)))
               (:day (incf day value))
               (:hour (incf hour value))
               (:minute (incf minute value))
               (:second (incf second value))))
    (assert (zerop month)
            (month)
            "Month support not yet implemented in local-time-duration:duration.")
    (duration :day day
              :hour hour
              :minute minute
              :sec second)))

(defrule iso8601-date-T-time
    (and iso8601-duration-date (? iso8601-duration-time))
  (:destructure (date time)
    (make-duration-from-time-values (append date time))))

(defrule iso8601-week-W
    (and iso8601-quantity #\W)
  (:lambda (production)
    (make-duration-from-time-values (list :week (parse-integer (first production) :radix 10)))))

(defun char-string-to-integer (chars)
  (parse-integer (apply #'concatenate 'string chars) :radix 10))

(defun production-ymd-to-list (year month day)
  (list :year (char-string-to-integer year)
        :month (char-string-to-integer month)
        :day (char-string-to-integer day)))

(defun production-hms-to-list (hour minute second)
  (list :hour (char-string-to-integer hour)
        :minute (char-string-to-integer minute)
        :second (char-string-to-integer second)))

(defrule iso8601-date-full
    (and (and digit digit digit digit) #\- (and digit digit) #\- (and digit digit))
  (:lambda (production)
    (production-ymd-to-list (first production) (third production) (fifth production))))

(defrule iso8601-time-full
    (and (and digit digit) #\: (and digit digit) #\: (and digit digit))
  (:lambda (production)
    (production-hms-to-list (first production) (third production) (fifth production))))

(defrule iso8601-date-time-full
    (and iso8601-date-full #\T iso8601-time-full)
  (:destructure (date _T time)
    (declare (ignore _T))
    (make-duration-from-time-values (append date time))))

(defrule iso8601-date-compact
    (and (and digit digit digit digit) (and digit digit) (and digit digit))
  (:destructure (year month day)
    (production-ymd-to-list year month day)))

(defrule iso8601-time-compact
    (and (and digit digit) (and digit digit) (and digit digit))
  (:destructure (hour minute second)
    (production-hms-to-list hour minute second)))

(defrule iso8601-date-time-compact
    (and iso8601-date-compact #\T iso8601-time-compact)
  (:destructure (date _T time)
    (declare (ignore _T))
    (make-duration-from-time-values (append date time))))

(defrule iso8601-duration
    (and #\P
         (or iso8601-date-time-full
             iso8601-date-time-compact
             iso8601-date-T-time
             iso8601-week-W))
  (:destructure (P duration)
    (declare (ignore P))
    duration))

(defun parse-iso8601-duration (string)
  "Parser for ISO8601 durations (with limitations) returning DURATION instances.

http://en.wikipedia.org/wiki/ISO_8601#Durations

The only, known, divergence from the syntax specified is that
fractional values are allowed anywhere and not only in the smallest
value."
  (esrap:parse 'iso8601-duration string))

(defun format-iso8601-duration (destination duration)
  (with-designated-stream (stream destination)
    (multiple-value-bind (nsecs secs minutes hours days weeks months years)
        (decode-duration duration :weeks nil)
      (declare (ignore weeks months years))
      (write-string "P0Y0M" stream)
      (format stream "~DDT~DH~DM~AS" days hours minutes (pretty-seconds secs nsecs)))))
