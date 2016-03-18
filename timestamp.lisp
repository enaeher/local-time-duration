(in-package :ltd)

(defun timestamp-difference (time-a time-b)
  "Returns a duration representing the time elapsed between the timestamps `TIME-A` and `TIME-B`. This duration may be negative if `TIME-B` is later than `TIME-A`."
  (let ((seconds (- (lt:timestamp-to-universal time-a) (lt:timestamp-to-universal time-b))))
    (duration :sec seconds)))

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
