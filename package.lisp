;;;; package.lisp

(defpackage #:gooptest
  (:nicknames #:goop)
  (:use #:cl #:alexandria)
  (:export #:with-core
           #:cycles
           #:cycles-between
           #:time-cycles
           #:pin

           ;; core generic functions
           #:core-inc-elapsed
           #:core-one-cycle
           #:core-many-cycles
           #:core-frequency
           #:core-pin
           #:core-set-pin
           #:core-reset

           ;; non-functions
           #:core                       ; class
           #:*core*                     ; context variable
           ))

(defpackage #:gooptest-avr
  (:nicknames #:goop-avr)
  (:use #:cl #:gooptest #:cffi)
  (:export #:make-core

           ;; non-functions
           #:avr-core                   ; class
           ))

(defpackage #:gooptest-arduino-avr
  (nicknames #:goopduino)
  (:use #:cl #:gooptest #:gooptest-avr)
  (:export #:make-arduino-uno

           ;; non-functions
           #:arduino-uno-core           ; class
           ))

(defpackage #:gooptest-examples
  (:use #:cl #:gooptest #:gooptest-avr #:gooptest-arduino-avr)
  (:export #:blink))
