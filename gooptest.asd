;;;; gooptest.asd

(asdf:defsystem #:gooptest
  :description "A microcontroller testing framework."
  :author "Mark Polyakov"
  :license  "GPL-3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:cl-autowrap #:alexandria #:trivial-garbage)
  :components ((:file "package")
               (:file "src/gooptest")
               (:static-file "src/simavr-proxy.h")
               (:file "src/gooptest-avr-cl-autowrap")
               (:file "src/gooptest-avr")
               (:file "src/gooptest-arduino-avr")
               ))
