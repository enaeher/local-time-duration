(in-package :ltd-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (local-time:enable-read-macros))

(5am:test timestamp-difference
  (5am:is (duration= (timestamp-difference @2014-01-01T09:00:00
                                           @2014-01-01T04:30:00)
                     (duration :hour 4 :minute 30))))

(5am:test timestamp-add-duration
  (5am:is (local-time:timestamp= (timestamp-duration+ @2014-01-01T09:00:00
                                                      (duration :hour 3))
                                 @2014-01-01T12:00:00)))

(5am:test timestamp-subtract-duration
  (5am:is (local-time:timestamp= (timestamp-duration- @2014-01-01T09:00:00
                                                      (duration :hour 3))
                                 @2014-01-01T06:00:00)))
