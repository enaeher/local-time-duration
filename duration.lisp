(in-package :ltd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +nsecs-per-second+ 1000000000)
  (defconstant +nsecs-per-minute+ (* +nsecs-per-second+ local-time:+seconds-per-minute+))
  (defconstant +nsecs-per-hour+ (* +nsecs-per-second+ local-time:+seconds-per-hour+))
  (defconstant +nsecs-per-day+ (* +nsecs-per-second+ local-time:+seconds-per-day+))
  (defconstant +nsecs-per-week+ (* +nsecs-per-day+ local-time:+days-per-week+)))

(defclass duration ()
  ((day :accessor day-of :initarg :day :initform 0 :type integer)
   (sec :accessor sec-of :initarg :sec :initform 0 :type integer)
   (nsec :accessor nsec-of :initarg :nsec :initform 0 :type (integer 0 999999999)))
  (:documentation "A duration instance represents a period of time with no additional context (e.g., starting or ending time or location)."))

(defun human-readable-duration (duration stream)
  (multiple-value-bind (weeks remaining)
      (duration-as duration :week)
    (multiple-value-bind (days remaining)
        (duration-as remaining :day)
      (multiple-value-bind (hours remaining)
          (duration-as remaining :hour)
        (multiple-value-bind (minutes remaining)
            (duration-as remaining :minute)
          (multiple-value-bind (secs remaining)
              (duration-as remaining :sec)
            (let ((nsecs (duration-as remaining :nsec)))
              (flet ((zero-is-nil (x) (if (zerop x) nil x)))
                (if (every #'zerop (list weeks days hours minutes secs nsecs))
                    (format stream "0 length")
                    (format stream "~@[~d week~:p~]~@[ ~d day~:p~]~@[ ~d hour~:p~]~@[ ~d minute~:p~]~@[ ~d second~:p~]~@[ ~d nsec~:p~]"
                            (zero-is-nil weeks)
                            (zero-is-nil days)
                            (zero-is-nil hours)
                            (zero-is-nil minutes)
                            (zero-is-nil secs)
                            (zero-is-nil (nsec-of remaining))))))))))))

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

(defmethod print-object ((object duration) stream)
  (print-unreadable-object (object stream :type 'duration)
    (format stream "[~d/~d/~d] "
            (day-of object)
            (sec-of object)
            (nsec-of object))
    (human-readable-duration object stream)))

(defun duration (&key (week 0) (day 0) (hour 0) (minute 0) (sec 0) (nsec 0))
  "Returns a new duration instance representing the sum of the `WEEK`, `DAY`, `HOUR`, `MINUTE`, `SEC`, and `NSEC` arguments. Durations are normalized, that is, (duration :hour 1) and (duration :minute 60) will result in duration instances with the same internal representation."
  (let ((total-nsecs (+ nsec
                        (* +nsecs-per-second+ sec)
                        (* +nsecs-per-minute+ minute)
                        (* +nsecs-per-hour+ hour)
                        (* +nsecs-per-day+ day)
                        (* +nsecs-per-week+ week))))
    (multiple-value-bind (normalized-days remaining-nsecs)
        (floor total-nsecs +nsecs-per-day+)
      (multiple-value-bind (normalized-seconds remaining-nsecs)
          (floor remaining-nsecs +nsecs-per-second+)
        (make-instance 'duration
                       :day normalized-days
                       :sec normalized-seconds
                       :nsec remaining-nsecs)))))

(defun %duration-compare (a b)
  (declare (type duration a b))
  (cond
    ((< (day-of a) (day-of b)) '<)
    ((> (day-of a) (day-of b)) '>)
    ((< (sec-of a) (sec-of b)) '<)
    ((> (sec-of a) (sec-of b)) '>)
    ((< (nsec-of a) (nsec-of b)) '<)
    ((> (nsec-of a) (nsec-of b)) '>)
    (t '=)))

(%defcomparator duration< ('duration)
  (eql (%duration-compare a b) '<))

(setf (documentation #'duration< 'function) "Returns `T` if every duration is shorter than the preceding duration, else returns `NIL`.")

(%defcomparator duration<= ('duration)
  (not (null (member (%duration-compare a b) '(< =)))))

(setf (documentation #'duration<= 'function) "Returns `T` if every duration is shorter than or equal to the preceding duration, else returns `NIL`.")

(%defcomparator duration> ('duration)
  (eql (%duration-compare a b) '>))

(setf (documentation #'duration> 'function) "Returns `T` if every duration is longer than the preceding duration, else returns `NIL`.")

(%defcomparator duration>= ('duration)
  (not (null (member (%duration-compare a b) '(> =)))))

(setf (documentation #'duration>= 'function) "Returns `T` if every duration is longer than or equal to the preceding duration, else returns `NIL`.")

(%defcomparator duration= ('duration)
  (eql (%duration-compare a b) '=))

(setf (documentation #'duration= 'function) "Returns `T` if every duration is equally long, else returns `NIL`.")

(%defcomparator duration/= ('duration)
  (not (eql (%duration-compare a b) '=)))

(setf (documentation #'duration/= 'function) "Returns `T` if every duration is not equally long, else returns `NIL`.")

(defun duration-as (duration unit)
  "Returns two values: the first is the number of whole `UNIT`s within `DURATION`, and the second is a fresh duration representing the reamainder of the original duration after dividing it by `UNIT`. `UNIT` must be one of :week, :day, :hour, :minute, :sec, and :nsec."
  (declare (type duration duration))
  (macrolet ((divide-storing-remainder (dividend divisor place)
               `(multiple-value-bind (quotient remainder)
                    (floor ,dividend ,divisor)
                  (setf ,place remainder)
                  quotient)))
    (let* (remaining-days
           remaining-secs
           remaining-nsecs
           (whole-units
            (ecase unit
              (:week (+ (divide-storing-remainder (day-of duration) local-time:+days-per-week+ remaining-days)
                        (divide-storing-remainder (sec-of duration) local-time:+seconds-per-day+ remaining-secs)
                        (divide-storing-remainder (nsec-of duration) +nsecs-per-day+ remaining-nsecs)))
              (:day (+ (day-of duration)
                       (divide-storing-remainder (sec-of duration) local-time:+seconds-per-day+ remaining-secs)
                       (divide-storing-remainder (nsec-of duration) +nsecs-per-day+ remaining-nsecs)))
              (:hour (+ (* (day-of duration) local-time:+hours-per-day+)
                        (divide-storing-remainder (sec-of duration) local-time:+seconds-per-hour+ remaining-secs)
                        (divide-storing-remainder (nsec-of duration) +nsecs-per-hour+ remaining-nsecs)))
              (:minute (+ (* (day-of duration) local-time:+minutes-per-day+)
                          (divide-storing-remainder (sec-of duration) local-time:+seconds-per-minute+ remaining-secs)
                          (divide-storing-remainder (nsec-of duration) +nsecs-per-minute+ remaining-nsecs)))
              (:sec (+ (* (day-of duration) local-time:+seconds-per-day+)
                       (sec-of duration)
                       (divide-storing-remainder (nsec-of duration) +nsecs-per-second+ remaining-nsecs)))
              (:nsec (+ (* (day-of duration) +nsecs-per-day+)
                        (* (sec-of duration) +nsecs-per-second+)
                        (nsec-of duration))))))
      (values whole-units (duration :day (or remaining-days 0) :sec (or remaining-secs 0) :nsec (or remaining-nsecs 0))))))

(defun duration+ (&rest durations)
  "Returns a fresh duration representing the sum of the lengths of its arguments."
  (let ((total-day (reduce #'+ durations :key #'day-of))
        (total-sec (reduce #'+ durations :key #'sec-of))
        (total-nsec (reduce #'+ durations :key #'nsec-of)))
    (duration :day total-day :sec total-sec :nsec total-nsec)))

(defun duration- (&rest durations)
  "Returns a fresh duration representing the result of subtracting the length of each argument in turn."
  (let ((total-day (reduce #'- durations :key #'day-of))
        (total-sec (reduce #'- durations :key #'sec-of))
        (total-nsec (reduce #'- durations :key #'nsec-of)))
    (duration :day total-day :sec total-sec :nsec total-nsec)))

(defun duration* (duration factor)
  "Returns a fresh duration as long as `DURATION` multiplied by `FACTOR`."
  (let ((curried-* (alexandria:rcurry #'* factor)))
    (let ((total-day (funcall curried-* (day-of duration)))
          (total-sec (funcall curried-* (sec-of duration)))
          (total-nsec (funcall curried-* (nsec-of duration))))
      (duration :day total-day :sec total-sec :nsec total-nsec))))

(defun duration/ (duration divisor)
  "Returns a fresh duration that is as long as `DURATION` divided by `DIVISOR`."
  (let ((curried-/ (alexandria:rcurry #'/ divisor)))
    (let ((total-day (funcall curried-/ (day-of duration)))
          (total-sec (funcall curried-/ (sec-of duration)))
          (total-nsec (funcall curried-/ (nsec-of duration))))
      (duration :day total-day :sec total-sec :nsec total-nsec))))

(defun duration-minimum (duration &rest durations)
  (local-time::contest #'duration< (cons duration durations)))

(defun duration-maximum (duration &rest durations)
  (local-time::contest #'duration> (cons duration durations)))
