(in-package :ltd)

(defun timestamp-difference (time-a time-b)
  (let ((day (- (local-time:day-of time-a)
                (local-time:day-of time-b)))
        (sec (- (local-time:sec-of time-a)
                (local-time:sec-of time-b)))
        (nsec (- (local-time:nsec-of time-a)
                 (local-time:nsec-of time-b))))
    (duration :day day :sec sec :nsec nsec)))

(defun timestamp-duration+ (timestamp duration)
  (local-time:adjust-timestamp timestamp
    (offset :day (day-of duration))
    (offset :sec (sec-of duration))
    (offset :nsec (nsec-of duration))))

(defun timestamp-duration- (timestamp duration)
  (local-time:adjust-timestamp timestamp
    (offset :day (- (day-of duration)))
    (offset :sec (- (sec-of duration)))
    (offset :nsec (- (nsec-of duration)))))
