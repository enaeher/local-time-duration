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
