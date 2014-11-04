(in-package :ltd)

(defun timestamp-difference (time-a time-b)
  "Returns a duration representing the time elapsed between the timestamps `TIME-A` and `TIME-B`. This duration may be negative if `TIME-B` is later than `TIME-A`."
  (let ((day (- (local-time:day-of time-a)
                (local-time:day-of time-b)))
        (sec (- (local-time:sec-of time-a)
                (local-time:sec-of time-b)))
        (nsec (- (local-time:nsec-of time-a)
                 (local-time:nsec-of time-b))))
    (incf sec (- (lt::timestamp-subtimezone time-a timezone)
		 (lt::timestamp-subtimezone time-b timezone)))
    (duration :day day :sec sec :nsec nsec)))

(defun timestamp-duration+ (timestamp duration)
  "Returns a fresh timestamp representing the time when `DURATION` has elapsed after `TIMESTAMP`."
  (local-time:adjust-timestamp timestamp
    (offset :day (day-of duration))
    (offset :sec (sec-of duration))
    (offset :nsec (nsec-of duration))))

(defun timestamp-duration- (timestamp duration)
  "Returns a fresh timestamp representing the time when `DURATION` will elapse before `TIMESTAMP`."
  (local-time:adjust-timestamp timestamp
    (offset :day (- (day-of duration)))
    (offset :sec (- (sec-of duration)))
    (offset :nsec (- (nsec-of duration)))))
