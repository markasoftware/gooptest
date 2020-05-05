(in-package :gooptest-avr)

(cffi:load-foreign-library "libelf.so")
(cffi:load-foreign-library "libsimavr.so")

(defclass avr-core (core)
  ((avr-ptr :accessor get-ptr :type avr-t)
   ))

(defun avr-ioctl-def (c1 c2 c3 c4)
  "See simavr/sim_io.h"
  (flet ((intify (arg)
           (etypecase arg (integer arg) (character (char-code arg)))))
    (+ (ash (intify c1) 24)
       (ash (intify c2) 16)
       (ash (intify c3) 8)
       (intify c4))))

(defmethod core-pin ((instance avr-core) p)
  "Pin should be a string designator like :a4 for port A, pin 4"
  (with-alloc (ioport-state 'avr-ioport-state-t)
    (let* ((port-char (elt (string-upcase p) 0))
           (pin-num (parse-integer (string p) :start 1))
           (ioctl-res
            (avr-ioctl (get-ptr instance) (avr-ioctl-def #\i #\o #\s port-char)
                       ioport-state))
           ;; TODO: NOT THIS NOT THIS NOT THIS FILE AN AUTOWRAP BUG
           (ioport-state-long (cffi:mem-ref (autowrap:ptr ioport-state) :unsigned-long))
           (ddr (mod (ash ioport-state-long -16) 255))
           (port (mod (ash ioport-state-long -8) 255))
           (is-output (plusp (logand (ash 1 pin-num) ddr)))
           (is-high (plusp (logand (ash 1 pin-num) port))))

      (unless (zerop ioctl-res)
        (error "Error getting pin state"))
      (if is-output (if is-high :high :low)
          (if is-high :pull-up :float)))))

(defmethod core-one-cycle ((instance avr-core))
  (avr-run (get-ptr instance))
  (call-next-method))

(defmethod initialize-instance :after
    ((instance avr-core) &key mcu firmware-path)
  (let ((firmware-namestring (namestring (truename firmware-path)))
        (mcu-name (string-downcase mcu))
        )

    (setf (get-ptr instance) (avr-make-mcu-by-name mcu-name))
    (when (cffi:null-pointer-p (ptr (get-ptr instance)))
      (error "Unknown mcu"))
    (avr-init (get-ptr instance))
    (with-alloc (firmware 'elf-firmware-t)
      (unless (zerop (elf-read-firmware firmware-namestring firmware))
        (error "Malformatted firmware"))
      (avr-load-firmware (get-ptr instance) firmware))

    ;; TODO
    ;; (trivial-garbage:finalize instance (lambda ()
    ;;                                      ()))

    ;; TODO: set frequency (and vcc?) using elf metadata if no initarg provided.
    ;; Also, throw errors when frequency is not present (from gooptest core).
    ))


