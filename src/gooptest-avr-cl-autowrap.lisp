(in-package #:gooptest-avr-cl-autowrap)

(c-include '(gooptest "src/simavr-proxy.h")
           ;; these work on my system
           :sysincludes '("/usr/include" "/usr/include/x86_64-linux-gnu")
           :exclude-sources ("/usr")
           :include-sources ("/usr/include/simavr")
           :spec-path '(gooptest))
