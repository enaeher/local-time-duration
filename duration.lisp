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
   (nsec :accessor nsec-of :initarg :nsec :initform 0 :type integer))
  (:documentation "A duration instance represents a period of time with no additional context (e.g., starting or ending time or location)."))

(defun duration (&key (week 0) (day 0) (hour 0) (minute 0) (sec 0) (nsec 0))
  "Returns a new duration instance representing the sum of the `WEEK`, `DAY`, `HOUR`, `MINUTE`, `SEC`, and `NSEC` arguments. Durations are normalized, that is, (duration :hour 1) and (duration :minute 60) will result in duration instances with the same internal representation."
  (let* ((total-nsecs (+ nsec
                         (* +nsecs-per-second+ sec)
                         (* +nsecs-per-minute+ minute)
                         (* +nsecs-per-hour+ hour)
                         (* +nsecs-per-day+ day)
                         (* +nsecs-per-week+ week)))
         (sign (if (plusp total-nsecs)
                   1
                   -1))
         (total-nsecs (abs total-nsecs)))
    (multiple-value-bind (normalized-days remaining-nsecs)
        (floor total-nsecs +nsecs-per-day+)
      (multiple-value-bind (normalized-seconds remaining-nsecs)
          (floor remaining-nsecs +nsecs-per-second+)
        (make-instance 'duration
                       :day (* sign normalized-days)
                       :sec (* sign normalized-seconds)
                       :nsec (* sign remaining-nsecs))))))

(defun decode-duration (duration &key (weeks nil))
  "Returns, as multiple values, DURATION's logical components:

 (nsecs secs minutes hours days weeks months years)

If WEEKS is T days the days component of DURATION will first be used
to compute an integral number of weeks, and the remainder days will be
returned. If WEEKS is NIL (the default) the weeks value will always be
0 and the whole number of days will be returned as DAYS.

In the current implementation MONTHS and YEARS are always 0."
  (multiple-value-bind (weeks remaining)
      (if weeks
          (duration-as duration :week)
          (values 0 duration))
    (multiple-value-bind (days remaining)
        (duration-as remaining :day)
      (multiple-value-bind (hours remaining)
          (duration-as remaining :hour)
        (multiple-value-bind (minutes remaining)
            (duration-as remaining :minute)
          (multiple-value-bind (secs remaining)
              (duration-as remaining :sec)
            (let ((nsecs (duration-as remaining :nsec)))
              (values nsecs
                      secs
                      minutes
                      hours
                      days
                      weeks
                      0                 ; months
                      0                 ; years
                      ))))))))

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
                    (truncate ,dividend ,divisor)
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
