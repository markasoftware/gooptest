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
           (ddr (mod (ash ioport-state-long -15) 256))
           (port (mod (ash ioport-state-long -7) 256))
           (is-output (plusp (logand (ash 1 pin-num) ddr)))
           (is-high (plusp (logand (ash 1 pin-num) port))))

      (unless (zerop ioctl-res)
        (error "Error getting pin state"))
      (if is-output (if is-high :high :low)
          (if is-high :pull-up :float)))))

(defmethod core-one-cycle ((instance avr-core))
  (avr-run (get-ptr instance))
  (setf (core-elapsed instance) (avr-t.cycle (get-ptr instance)))
  )

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

;;;; ARDUINO

(defclass arduino-uno-core (avr-core)
  ())

(defmethod core-pin ((core arduino-uno-core) p)
  (let* ((uno-pins '(0 :d0
                     1 :d1
                     2 :d2
                     3 :d3
                     4 :d4
                     5 :d5
                     6 :d6
                     7 :d7
                     8 :b0
                     9 :b1
                     10 :b2
                     11 :b3
                     12 :b4
                     13 :b5
                     :a0 :c0
                     :a1 :c1
                     :a2 :c2
                     :a3 :c3
                     :a4 :c4
                     :a5 :c5))
         (translated-pin (or (getf uno-pins p) p)))
    (call-next-method core translated-pin)))

(defun make-arduino-uno (firmware-path &optional (firmware-type :sketch))
  (declare ((or pathname string) firmware-path))
  (let ((firmware-truename (truename firmware-path)))
    (ecase firmware-type
      ;; path to a sketch directory
      (:sketch
       ;; TODO: some Make-like trickery to avoid recompilations.
       (handler-case
           (uiop:run-program (list "arduino-cli" "compile" "-b" "arduino:avr:uno"
                                   (directory-namestring firmware-truename)))
         (uiop:subprocess-error (c)
           (if (= 127 (uiop:subprocess-error-code c))
               (error "arduino-cli is required to compile Arduino sketches.")
               ;; re-throw unknown errors; probably compilation problems
               (error c))))
       
       (setq firmware-truename
             (truename
              (merge-pathnames
               (format nil "~A~A.arduino.avr.uno.elf"
                       firmware-truename
                       (lastcar (pathname-directory firmware-truename)))
               firmware-truename))))
      ;; an elf file; just pass it on
      (:elf))

    (make-instance 'arduino-uno-core
                   :firmware-path firmware-truename
                   :frequency 16000000
                   :mcu :atmega328p)))

(setf (fdefinition 'make-arduino-nano) #'make-arduino-uno)
