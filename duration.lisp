(in-package :ltd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +nsecs-per-second+ 1000000000)
  (defconstant +nsecs-per-minute+ (* +nsecs-per-second+ local-time:+seconds-per-minute+))
  (defconstant +nsecs-per-hour+ (* +nsecs-per-second+ local-time:+seconds-per-hour+))
  (defconstant +nsecs-per-day+ (* +nsecs-per-second+ local-time:+seconds-per-day+)))

(defclass duration ()
  ((day :accessor day-of :initarg :day :initform 0 :type integer)
   (sec :accessor sec-of :initarg :sec :initform 0 :type integer)
   (nsec :accessor nsec-of :initarg :nsec :initform 0 :type (integer 0 999999999))))

(defmethod print-object ((object duration) stream)
  (print-unreadable-object (object stream :type 'duration)
    (multiple-value-bind (days remaining)
        (duration-as object :day)
      (multiple-value-bind (hours remaining)
          (duration-as remaining :hour)
        (multiple-value-bind (minutes remaining)
            (duration-as remaining :minute)
          (multiple-value-bind (secs remaining)
              (duration-as remaining :sec)
            (flet ((zero-is-nil (x) (if (zerop x) nil x)))
              (format stream "[~d/~d/~d]~@[ ~d day~:p~]~@[ ~d hour~:p~]~@[ ~d minute~:p~]~@[ ~d second~:p~]~@[ ~d nsec~:p~]"
                      (day-of object)
                      (sec-of object)
                      (nsec-of object)
                      (zero-is-nil days)
                      (zero-is-nil hours)
                      (zero-is-nil minutes)
                      (zero-is-nil secs)
                      (zero-is-nil (nsec-of remaining))))))))))

(defun duration (&key (day 0) (hour 0) (minute 0) (sec 0) (nsec 0))
  (let ((total-nsecs (+ nsec
                        (* +nsecs-per-second+ sec)
                        (* +nsecs-per-minute+ minute)
                        (* +nsecs-per-hour+ hour)
                        (* +nsecs-per-day+ day))))
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

;;; FIXME -- this is just copied from local-time with the type checking changed from timestamp to duration
(defmacro %defcomparator (name &body body)
  (let ((pair-comparator-name (intern (concatenate 'string "%" (string name)))))
    `(progn
      (declaim (inline ,pair-comparator-name))
      (defun ,pair-comparator-name (a b)
        (assert (typep a 'duration)
                nil
                'type-error
                :datum a
                :expected-type 'duration)
        (assert (typep b 'duration)
                nil
                'type-error
                :datum b
                :expected-type 'duration)
        ,@body)
      (defun ,name (&rest times)
        (declare (dynamic-extent times))
        (loop for head on times
              while (cdr head)
              always (,pair-comparator-name (first head) (second head))))
      (define-compiler-macro ,name (&rest times)
        (let ((vars (loop
                      :for i :upfrom 0 :below (length times)
                      :collect (gensym (concatenate 'string "TIME-" (princ-to-string i) "-")))))
          `(let (,@(loop
                     :for var :in vars
                     :for time :in times
                     :collect (list var time)))
            ;; we could evaluate comparisons of timestamp literals here
            (and ,@(loop
                     :for (a b) :on vars
                     :while b
                     :collect `(,',pair-comparator-name ,a ,b)))))))))

(%defcomparator duration<
  (eql (%duration-compare a b) '<))

(%defcomparator duration<=
  (not (null (member (%duration-compare a b) '(< =)))))

(%defcomparator duration>
  (eql (%duration-compare a b) '>))

(%defcomparator duration>=
  (not (null (member (%duration-compare a b) '(> =)))))

(%defcomparator duration=
  (eql (%duration-compare a b) '=))

(%defcomparator duration/=
  (not (eql (%duration-compare a b) '=)))

(defun duration-as (duration unit)
  "Returns two values: the first is the number of whole UNITs within DURATION, and the second is a new duration representing the reamainder of the original duration after dividing it by UNIT."
  (declare (type duration duration))
  (macrolet ((divide-storing-remainder (dividend divisor place)
               `(multiple-value-bind (quotient remainder)
                    (floor ,dividend ,divisor)
                  (setf ,place remainder)
                  quotient)))
    (let* (remaining-secs
           remaining-nsecs
           (whole-units
            (ecase unit
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
      (values whole-units (duration :sec (or remaining-secs 0) :nsec (or remaining-nsecs 0))))))

(defun duration+ (&rest durations)
  (let ((total-day (reduce #'+ durations :key #'day-of))
        (total-sec (reduce #'+ durations :key #'sec-of))
        (total-nsec (reduce #'+ durations :key #'nsec-of)))
    (duration :day total-day :sec total-sec :nsec total-nsec)))

(defun duration- (&rest durations)
  (let ((total-day (reduce #'- durations :key #'day-of))
        (total-sec (reduce #'- durations :key #'sec-of))
        (total-nsec (reduce #'- durations :key #'nsec-of)))
    (duration :day total-day :sec total-sec :nsec total-nsec)))

(defun duration* (duration factor)
  (let ((curried-* (alexandria:rcurry #'* factor)))
    (let ((total-day (funcall curried-* (day-of duration)))
          (total-sec (funcall curried-* (sec-of duration)))
          (total-nsec (funcall curried-* (nsec-of duration))))
      (duration :day total-day :sec total-sec :nsec total-nsec))))

(defun duration/ (duration divisor)
  (let ((curried-/ (alexandria:rcurry #'/ divisor)))
    (let ((total-day (funcall curried-/ (day-of duration)))
          (total-sec (funcall curried-/ (sec-of duration)))
          (total-nsec (funcall curried-/ (nsec-of duration))))
      (duration :day total-day :sec total-sec :nsec total-nsec))))
