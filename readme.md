
local-time-duration
===================

This is a simple library that provides a `duration` class, and some useful accompanying operations. It is built on and requires the [local-time](http://common-lisp.net/projects/local-time/) library.

Assumptions and limitations
---------------------------

`Duration`s describe a length of time--e.g., "4 days" or "5 hours and 29 minutes"--without reference to a particular start or end time. Unlike the [time-interval](https://github.com/enaeher/local-time-duration) library, `local-time-duration` makes no attempt to deal with variable-length units. In particular, `duration`s may neither be specified nor represented in terms of years or months. Also, since `duration`s do not represent a specific interval of time with a specific start or end time, there is no attempt when working exclusively with `duration`s to handle leap seconds, timezone or daylight savings time changes, etc. However, when adding or removing a `duration` to or from a `local-time` `timestamp`, the `local-time` library will correctly handle timezone/DST.