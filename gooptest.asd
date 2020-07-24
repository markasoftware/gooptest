;;;; gooptest.asd

(asdf:defsystem #:gooptest
  :description "A microcontroller testing framework."
  :author "Mark Polyakov"
  :license  "GPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:cffi #:cl-autowrap #:cl-plus-c #:alexandria #:uiop #:babel)
  :components ((:file "package")
               (:file "src/gooptest")
               (:static-file "src/simavr-proxy.h")
               (:file "src/gooptest-avr-cl-autowrap")
               (:file "src/gooptest-avr")

               (:module "autowrap-specs"
                :pathname "autowrap-specs"
                :components
                ((:static-file "simavr-proxy.x86_64-pc-linux-gnu.spec")
                 (:static-file "simavr-proxy.x86_64-apple-darwin9.spec")
                 (:static-file "simavr-proxy.x86_64-unknown-freebsd.spec")
                 (:static-file "simavr-proxy.x86_64-unknown-openbsd.spec")
                 (:static-file "simavr-proxy.i686-pc-linux-gnu.spec")
                 (:static-file "simavr-proxy.i686-apple-darwin9.spec")
                 (:static-file "simavr-proxy.i386-unknown-freebsd.spec")
                 (:static-file "simavr-proxy.i386-unknown-openbsd.spec")))))

(asdf:defsystem #:gooptest/examples
  :description "Examples for a microcontroller testing framework."
  :author "Mark Polyakov"
  :license "GPL-3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:gooptest)
  :pathname "examples"
  :components ((:file "example-setup")

               (:static-file "blink/blink.ino")
               (:file "blink")

               (:static-file "at-least-two/at-least-two.ino")
               (:file "at-least-two")

               (:static-file "analog-to-serial/analog-to-serial.ino")
               (:file "analog-to-serial")

               (:static-file "serial-spongebob/serial-spongebob.ino")
               (:file "serial-spongebob")
               ))
