
# local-time-duration

This is a simple library that provides a `duration` class, and some useful accompanying operations. It is built on and requires the [local-time](http://common-lisp.net/projects/local-time/) library.

## Assumptions and limitations


Durations describe a length of time&mdash;e.g., "4 days" or "5 hours and 29 minutes"&mdash;without reference to a particular start or end time. Unlike the [time-interval](https://github.com/enaeher/local-time-duration) library, `local-time-duration` makes no attempt to deal with variable-length units. In particular, durations may neither be specified nor represented in terms of years or months. Also, since durations do not represent a specific interval of time with a specific start or end time, there is no attempt when working exclusively with durations to handle leap seconds, timezone or daylight savings time changes, etc. However, when adding or removing a duration to or from a `local-time` timestamp, the `local-time` library will correctly handle timezone/DST.

## Examples

```
LTD> (duration :day 1 :minute 75)
#<DURATION [1/4500/0] 1 day 1 hour 15 minutes>

LTD> (duration= (duration :day 1) (duration :hour 24))
T

LTD> (duration+ (duration :hour 1) (duration :hour 23))
#<DURATION [1/0/0] 1 day>
```

A duration can be expressed in any arbitrary unit (the second value is the remainder of the duration that cannot be expressed in whole units):

```
LTD> (duration-as (duration :day 1 :hour 4 :minute 25) :hour)
28
#<DURATION [0/1500/0] 25 minutes>
```

`local-time-duration` also provides functions for working with `local-time` timestamps in conjunction with durations:

```
LTD> (timestamp-difference @2014-01-01T09:00:00 @2014-01-01T06:00:00)
#<DURATION [0/10800/0] 3 hours>
```