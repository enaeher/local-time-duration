(in-package :ltd-tests)

(5am:test duration-equality
  (5am:is (duration= (duration :day 1) (duration :hour 24))))

(5am:test duration-comparison
  (5am:is (duration< (duration :day 1) (duration :hour 48)))
  (5am:is (duration> (duration :day 1) (duration :hour 12))))

(5am:test duration-in-units
  (5am:is (eql (duration-as (duration :day 1) :sec) 86400)))

(5am:test duration-sum
  (5am:is (duration= (duration+ (duration :day 1)
                                (duration :hour 12)
                                (duration :minute 30))
                     (duration :sec 131400))))

(5am:test duration-difference
  (5am:is (duration= (duration- (duration :day 2)
                                (duration :day 1)
                                (duration :hour 12))
                     (duration :sec 43200))))

(5am:test duration-multiply
  (5am:is (duration= (duration* (duration :day 1)
                                2)
                     (duration :day 2))))

(5am:test duration-divide
  (5am:is (duration= (duration/ (duration :day 2)
                                2)
                     (duration :day 1))))

(5am:test duration-minimum
  (5am:is (duration= (duration-minimum (duration :day 2)
                                       (duration :hour 36)
                                       (duration :day 4))
                     (duration :hour 36))))

(defun gen-timestamp ()
  (lambda ()
    (flet ((rand-in-range (range-size)
             (- (random range-size) (/ range-size 2))))
      (local-time:adjust-timestamp (local-time:now)
        (:offset :year (rand-in-range 40))
        (:offset :month (rand-in-range 24))
        (:offset :day (rand-in-range 180))
        (:offset :minute (rand-in-range 600))
        (:offset :sec (rand-in-range 3600))
        (:offset :nsec (rand-in-range (expt 10 9)))))))

(5am:test duration-associates
  "Test that, for any pair of timestamps, this always holds:

  (+ b (difference a b)) == a"
  (let ((lt:*default-timezone* lt:+utc-zone+))
    (5am:for-all ((a (gen-timestamp))
                  (b (gen-timestamp)))
      (5am:is (lt:timestamp= a (ltd:timestamp-duration+ b (ltd:timestamp-difference a b)))))))
