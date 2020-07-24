;;;; package.lisp

(defpackage #:gooptest
  (:nicknames #:goop)
  (:use #:cl #:alexandria)
  (:export #:with-core
           #:cycles
           #:cycles-between
           #:elapsed
           #:until-pin
           #:pin-duty-cycle
           #:pin
           #:set-pin-digital
           #:set-pin-analog
           #:assert-pin
           #:uart-start
           #:uart-stop
           #:uart-data
           #:uart-send-byte
           #:uart-send
           #:uart-string
           #:until-uart
           #:set-spi-handler
           #:spi-handle

           #:defsuite
           #:in-suite
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
           #:core-uart-send
           #:core-uart-default-channel

           ;; non-functions
           #:core                       ; class
           #:*core*                     ; context variable
           #:*suite*
           #:*uart-baudrate*
           #:*uart-byte-size*
           #:pin-output                 ; type
           ))

(defpackage #:gooptest-avr-cl-autowrap
  (:use #:cl #:autowrap))

(defpackage #:gooptest-avr
  (:nicknames #:goop-avr)
  (:use #:cl #:alexandria #:gooptest #:gooptest-avr-cl-autowrap #:autowrap
        #:plus-c)
  (:export #:make-core

           #:load-foreign-libraries

           #:make-arduino-uno
           #:make-arduino-nano

           ;; non-functions
           #:avr-core                   ; class

           #:*gdb-port*
           ))
