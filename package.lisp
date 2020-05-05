;;;; package.lisp

(defpackage #:gooptest
  (:nicknames #:goop)
  (:use #:cl #:alexandria)
  (:export #:with-core
           #:cycles
           #:cycles-between
           #:time-cycles
           #:pin
           #:set-pin

           #:make-pin
           ;; pin slots are private -- set them in the constructor, doofus!
           ;; We already exported the pin symbol, which names the structure.

           ;; Useful core functions
           #:core-elapsed
           #:core-inc-elapsed
           #:core-vcc
           ;; Generic core functions (should be implemented by cores)
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
           ))

(defpackage #:gooptest-arduino-avr
  (:nicknames #:goopduino)
  (:use #:cl #:gooptest #:gooptest-avr)
  (:export #:make-arduino-uno

           ;; non-functions
           #:arduino-uno-core           ; class
           ))

(defpackage #:gooptest-examples
  (:use #:cl #:gooptest #:gooptest-avr #:gooptest-arduino-avr)
  (:export #:blink))
