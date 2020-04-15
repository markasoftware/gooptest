;;;; gooptest.asd

(asdf:defsystem #:gooptest
  :description "Describe gooptest here"
  :author "Mark Polyakov"
  :license  "GPL-3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:alexandria)
  :components ((:file "package")
               (:file "gooptest")
               (:file "gooptest-avr")
               (:file "gooptest-arduino-avr")))
