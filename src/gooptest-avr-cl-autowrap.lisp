(in-package #:gooptest-avr-cl-autowrap)

(c-include '(gooptest "src/simavr-proxy.h")
           ;; for debian 10 buster
           :sysincludes '("/usr/include" "/usr/include/x86_64-linux-gnu")
           :exclude-sources ("/usr")
           :include-sources ("/usr/include/x86_64-linux-gnu/bits/stdint.*"
                             "/usr/include/x86_64-linux-gnu/bits/types.h"
                             "/usr/include/simavr")
           :exclude-arch ("i686-pc-windows-msvc"
                          "x86_64-pc-windows-msvc")
           :no-accessors t
           :spec-path '(gooptest autowrap-specs))
