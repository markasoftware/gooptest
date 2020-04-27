;;;; gooptest.asd

(asdf:defsystem #:gooptest
  :description "Describe gooptest here"
  :author "Mark Polyakov"
  :license  "GPL-3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:alexandria #:trivial-garbage)
  :components ((:file "package")
               (:file "src/gooptest")
               (:file "src/gooptest-avr")
               (:file "src/gooptest-arduino-avr")))
