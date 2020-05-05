(in-package :gooptest-avr)

(cffi:load-foreign-library "libelf.so")
(cffi:load-foreign-library "libsimavr.so")

;; We have to manually keep track of pin changes. Although simavr internally
;; does record the states of all the pins, in registers, there's no consistent
;; way to access these across different cores. It might be possible to access
;; these registers directly, but we'd need to wrap the individual structs for
;; each core from the cores/ directory...at the end of the day it's easier and
;; more correct (in terms of what is and isn't public from the simavr API) to
;; just add custom listeners on all pins.

(defparameter *core-ports-alist*
  '(((atmega328 atmega328p atmega168 atmega168p atmega168pa atmega88 atmega88p
      atmega88pa atmega48 atmega48p atmega48pa)
     b c d)
    ((atmega1284 atmega1284p atmega164 atmega164p atmega164pa atmega324
      atmega324p atmega644 atmega644p)
     a b c d)
    ((atmega8 atmega8l atmega16 atmega32) b c d)
    ((atmega16m1 atmega64m1) b c d e)
    ;; the atmega128l cannot be instantiated because the L is capitalized in
    ;; simavr. No biggie; the 128 is exactly the same functionally (as far as
    ;; simavr is concerned). Interestingly, other parts with an l at the end are
    ;; usually lowercase in simavr.
    ((atmega128 atmega128l) a b c d e f g)
    (atmega1280 a b c d e f g h j k l)
    (atmega1281 a b c d e f g)
    (atmega32u4 b c d e f)
    ((atmega128rfa1 atmega128rfr2) b d e f g)
    (atmega169p a b c d e f g)
    ;; fuck that's a lot of ports
    (atmega2560 a b c d e f g h j k l)
    (atmega32u4 b c d e f)

    ((attiny24 attiny44 attiny84) a b)
    ((attiny25 attiny45 attiny85) b)
    ((attiny13 attiny13a) b)
    ((attiny2313 attiny2313v attiny2313a) a b d)
    (attiny4313 a b d)

    (at90usb162 b c d)
    ))

(defclass avr-core (core)
  ((ports :accessor get-ports :type cons) ; meh type
   (avr-ptr :accessor get-ptr :type avr-t)
   ))

(defmethod initialize-instance :after
    ((instance avr-core) &key mcu firmware-path)
  (let ((firmware-namestring (namestring (truename firmware-path)))
        (mcu-name (string-downcase mcu))
        (port-names (cdr (find-if
                          (compose (curry #'member-if (curry #'string-equal mcu))
                                   #'ensure-list #'car)
                          *core-ports-alist*))))
    (unless port-names
      ;; TODO: proper error/condition
      (error "Unknown mcu name"))

    (setf (get-ptr instance) (avr-make-mcu-by-name mcu-name))
    (avr-init (get-ptr instance))
    (with-alloc (firmware 'elf-firmware-t)
      (elf-read-firmware firmware-namestring firmware)
      (avr-load-firmware (get-ptr instance) firmware))

    ;; TODO
    ;; (trivial-garbage:finalize instance (lambda ()
    ;;                                      ()))

    ;; TODO: set frequency (and vcc?) using elf metadata if no initarg provided.
    ;; Also, throw errors when frequency is not present (from gooptest core).

    (setf (get-ports instance)
          (mapcar (lambda (p)
                    ;; alist of port names to pin statuses
                    (cons p
                          (loop for i from 1 to 8 collect (make-pin))))
                  port-names))))
