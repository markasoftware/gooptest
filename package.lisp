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
           #:assert-pin

           #:runsuite
           #:runtest

           ;; Useful core functions
           #:core-elapsed
           #:core-inc-elapsed
           #:core-vcc
           ;; Generic core functions (should be implemented by cores)
           #:core-one-cycle
           #:core-many-cycles
           #:core-frequency
           #:core-pin
           #:core-set-pin-digital
           #:core-set-pin-analog
           #:core-reset

           ;; non-functions
           #:core                       ; class
           #:*core*                     ; context variable
           ))

(defpackage #:gooptest-avr-cl-autowrap
  (:use #:cl #:autowrap)
  (:export #:avr-t
           #:avr-t.frequency
           #:avr-make-mcu-by-name
           #:avr-init
           #:elf-firmware-t
           #:avr-load-firmware

           #:avr-register-notify-hook
           ))

(defpackage #:gooptest-avr
  (:nicknames #:goop-avr)
  (:use #:cl #:alexandria #:gooptest #:gooptest-avr-cl-autowrap #:autowrap)
  (:export #:make-core

           ;; non-functions
           #:avr-core                   ; class

           #:make-arduino-uno
           #:make-arduino-nano
           ))
