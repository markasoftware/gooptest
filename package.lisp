;;;; package.lisp

(defpackage #:gooptest
  (:nicknames #:goop)
  (:use #:cl #:alexandria)
  (:export #:with-core
           #:cycles
           #:cycles-between
           #:until-pin
           #:pin-duty-cycle
           #:pin
           #:set-pin-digital
           #:set-pin-analog
           #:assert-pin
           #:uart-start
           #:uart-stop
           #:uart-data
           #:uart-string
           #:until-uart

           #:runsuite
           #:runtest

           ;; Useful core functions
           #:core-elapsed
           #:core-vcc
           #:core-frequency
           #:core-uart-buffers
           ;; Generic core functions (should be implemented by cores)
           #:core-one-cycle
           #:core-many-cycles
           #:core-pin
           #:core-set-pin-digital
           #:core-set-pin-analog
           #:core-uart-start
           #:core-uart-stop
           #:core-uart-data

           ;; non-functions
           #:core                       ; class
           #:*core*                     ; context variable
           #:pin-output                 ; type
           ))

(defpackage #:gooptest-avr-cl-autowrap
  (:use #:cl #:autowrap))
;; (c-include) exports everything, for better or (probably) for worse. TODO:
;; look into that, and removing some of the symbols we don't need in the first
;; place.

(defpackage #:gooptest-avr
  (:nicknames #:goop-avr)
  (:use #:cl #:alexandria #:gooptest #:gooptest-avr-cl-autowrap #:autowrap)
  (:export #:make-core

           ;; non-functions
           #:avr-core                   ; class

           #:make-arduino-uno
           #:make-arduino-nano
           ))
