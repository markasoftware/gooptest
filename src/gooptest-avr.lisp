(in-package :gooptest-avr)

(cffi:load-foreign-library "libelf.so")
(cffi:load-foreign-library "libsimavr.so")

(defclass avr-core (core)
  ((avr-ptr :accessor get-ptr :type avr-t)
   (mcu-name :accessor get-mcu-name :type keyword)
   (uarts :accessor get-uarts :initform (loop for i from 0 to 9 collect nil)
          :documentation "Each uart is related to its index in this list. Each
item starts as nil then becomes a list where the first element is a byte vector
and the second is a boolean indicating xon. Not a cons because more attributes
may be needed in the future. TODO: what are those attributes?")))

(defun avr-ioctl-def (c1 c2 c3 c4)
  "See simavr/sim_io.h"
  (flet ((intify (arg)
           (etypecase arg (integer arg) (character (char-code arg)))))
    (+ (ash (intify c1) 24)
       (ash (intify c2) 16)
       (ash (intify c3) 8)
       (intify c4))))

(defmacro with-pin (port-var pin-var pin-designator &body body)
  "Bind port-var to the upper case character of the port and pin-var to the
numerical (0-7) pin number inside that port."
  (once-only (pin-designator)
    `(let ((,port-var (elt (string-upcase ,pin-designator) 0))
           (,pin-var (parse-integer (string ,pin-designator) :start 1)))
       ,@body)))

(defmethod core-pin ((instance avr-core) p)
  "Pin should be a string designator like :a4 for port A, pin 4"
  (with-alloc (ioport-state 'avr-ioport-state-t)
    (with-pin port-char pin-num p
      (let* ((ioctl-res
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
           (if is-high :pull-up :float))))))

(defmethod core-set-pin-digital ((instance avr-core) p high)
  ;; unconditionally dispatch an IRQ. TODO: improve performance by caching the
  ;; IRQ object per port?
  (with-pin port-char pin-num p
    (avr-raise-irq (avr-io-getirq (get-ptr instance)
                                  (avr-ioctl-def #\i #\o #\g port-char)
                                  pin-num)
                   (if high 1 0))))

;; There's no logical connection between ports/pins and their ADC channels
(defparameter *adc-pin-plist*
  '(:atmega328 #1=(:c0 0 :c1 1 :c2 2 :c3 3 :c4 4 :c5 5)
    :atmega328p #1#
    ))

(defmethod core-set-pin-analog ((instance avr-core) p voltage)
  ;; For analog only, the pin may be, for example, :adc3 to indicate the adc
  ;; channel instead of the pin. There is no logical connection between pins and
  ;; adc channels in simavr.
  (declare (number voltage))
  (let ((adc-channel
         (if (starts-with-subseq "ADC" (string-upcase p))
             (parse-integer (string p) :start 3)
             (getf (getf *adc-pin-plist* (get-mcu-name instance))
                   (intern (string-upcase p) :keyword)))))

    (assert adc-channel)
    (avr-raise-irq (avr-io-getirq (get-ptr instance)
                                  (avr-ioctl-def #\a #\d #\c #\ )
                                  adc-channel)
                   (floor (* 1000 voltage)))))

(defmethod core-uart-default-channel ((instance avr-core))
  (declare (ignore instance))
  0)

(defmacro make-irq-callback (value-arg &body body)
  "Make a valid callback for an IRQ. Every element of the body should be quoted.
This is so you can embed values into the callback. Expands into a form that
defines the callback at runtime and evaluates to the callback itself (from
cffi:get-callback)"
  (with-gensyms (meta-gensym irq-arg param-arg)
    `(with-gensyms (,meta-gensym)
       (eval
        `(cffi:defcallback ,,meta-gensym :void
             ((,',irq-arg :pointer)
              (,',value-arg :uint32)
              (,',param-arg :pointer))
           (declare (ignore ,',irq-arg ,',param-arg))
           ,,@body))
       (cffi:get-callback ,meta-gensym))))

(defun channel-ioctl (channel)
  (declare ((integer 0 9) channel))
  (avr-ioctl-def #\u #\a #\r (elt (write-to-string channel) 0)))

(defmethod core-uart-start ((instance avr-core) channel)
  (declare ((integer 0 9) channel))
  ;; don't do anything if it's already started
  (symbol-macrolet ((uart
                     (nth channel (get-uarts instance))))
    (unless uart
      (setf uart (list
                  (make-array 0
                              :element-type '(unsigned-byte 8)
                              :adjustable t
                              :fill-pointer t)
                  t))

      (avr-irq-register-notify
       (avr-io-getirq (get-ptr instance)
                      (channel-ioctl channel)
                      ;; +uart-irq-output+
                      1)
       (make-irq-callback value
         `(vector-push-extend
           (coerce value '(unsigned-byte 8))
           ;; TODO: figure out how to use the uart symbol macro here without
           ;; breaking
           (first (nth ,channel (get-uarts ,instance)))))
       (cffi:null-pointer))

      (avr-irq-register-notify
       (avr-io-getirq (get-ptr instance)
                      (channel-ioctl channel)
                      ;; +uart-irq-xon+
                      2)
       (make-irq-callback value
         '(declare (ignore value))
         `(setf (second (nth ,channel (get-uarts ,instance))) t))
       (cffi:null-pointer))

      (avr-irq-register-notify
       (avr-io-getirq (get-ptr instance)
                      (channel-ioctl channel)
                      ;; +uart-irq-xoff+
                      3)
       (make-irq-callback value
         '(declare (ignore value))
         `(setf (second (nth ,channel (get-uarts ,instance))) nil))
       (cffi:null-pointer))
      )))

(defmethod core-uart-data ((instance avr-core) channel)
  (declare ((integer 0 9) channel))
  (first (nth channel (get-uarts instance))))
  
(defmethod core-uart-send ((instance avr-core) byte channel)
  (declare ((unsigned-byte 8) byte)
           ((integer 0 9) channel))
  (when (second (nth channel (get-uarts instance)))
    (avr-raise-irq
     (avr-io-getirq (get-ptr instance)
                    (channel-ioctl channel)
                    ;; TODO: replace with +uart-irq-input+
                    0)
     byte)
    t))

(defmethod core-one-cycle ((instance avr-core))
  (avr-run (get-ptr instance))
  (setf (core-elapsed instance) (avr-t.cycle (get-ptr instance)))
  )

(cffi:defcallback cycle-timer-callback
    :uint64 ((avr :pointer) (when :uint64) (param :pointer))
  (declare (ignore avr when param))
  0)

(defmethod core-many-cycles ((instance avr-core) n)

  (symbol-macrolet ((avr (get-ptr instance)))
    ;; probably don't need this check
    (when (plusp n)
      ;; TODO: off-by-one errors, make sure we reset cycle limit appropriately
      ;; at the end, etc.
      (setf (avr-t.run-cycle-limit avr) n)
      (avr-cycle-timer-register avr
                                n
                                (cffi:callback cycle-timer-callback)
                                (cffi:null-pointer))
      (call-next-method)
      (setf (avr-t.run-cycle-limit avr) 1)
      )))

(defmethod initialize-instance :after
    ((instance avr-core) &key mcu firmware-path frequency vcc)

  (let ((firmware-namestring (namestring (truename firmware-path)))
        (mcu-name (string-downcase mcu))
        )

    (setf (get-mcu-name instance) (intern (string-upcase mcu) :keyword))
    (setf (get-uarts instance) (loop for i from 0 to 9 collect nil))

    (setf (get-ptr instance) (avr-make-mcu-by-name mcu-name))
    (when (cffi:null-pointer-p (ptr (get-ptr instance)))
      (error "Unknown mcu"))
    (avr-init (get-ptr instance))
    (with-alloc (firmware 'elf-firmware-t)
      (unless (zerop (elf-read-firmware firmware-namestring firmware))
        (error "Malformatted firmware"))
      (avr-load-firmware (get-ptr instance) firmware))

    (setf (avr-t.frequency (get-ptr instance)) frequency)
    (when vcc
      (setf (avr-t.vcc (get-ptr instance)) (floor (* 1000 vcc)))
      (setf (avr-t.avcc (get-ptr instance)) (floor (* 1000 vcc)))
      )

    ;; TODO
    ;; (trivial-garbage:finalize instance (lambda ()
    ;;                                      ()))

    ;; TODO: set frequency (and vcc?) using elf metadata if no initarg provided.
    ))

;;;; ARDUINO

(defclass arduino-uno-core (avr-core)
  ())

(defun translate-pin-uno (arduino-pin)
  (or (getf '(0 :d0
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
              :a5 :c5) arduino-pin) arduino-pin))

(defmethod core-pin ((instance arduino-uno-core) p)
  (call-next-method instance (translate-pin-uno p)))

(defmethod core-set-pin-digital ((instance arduino-uno-core) p high)
  (call-next-method instance (translate-pin-uno p) high))

(defmethod core-set-pin-analog ((instance arduino-uno-core) p voltage)
  (call-next-method instance (translate-pin-uno p) voltage))

(defun maybe-compile-sketch (sketch-truename model)
  (let* ((sketch-name-only (lastcar (pathname-directory sketch-truename)))
         (.ino-truename
          (cond
            ((uiop:directory-exists-p sketch-truename)
             (format nil "~A~A.ino" sketch-truename sketch-name-only))
            ((uiop:file-exists-p sketch-truename)
             sketch-truename)
            ;; TODO: restart and prompt for new sketchname
            (t (error "Sketch not found"))))
         (.elf-truename
          (format nil "~A.arduino.avr.~A.elf" .ino-truename model))
         (.ino-mtime (file-write-date .ino-truename))
         (.elf-mtime (or
                      (and (probe-file .elf-truename)
                           (file-write-date .elf-truename))
                      0)))
    (when (< .elf-mtime .ino-mtime)
       (handler-case
           (uiop:run-program
            (list "arduino-cli" "compile" "-b" "arduino:avr:uno" .ino-truename))
         (uiop:subprocess-error (c)
           (if (= 127 (uiop:subprocess-error-code c))
               (error "arduino-cli is required to compile Arduino sketches.")
               ;; re-throw unknown errors; probably compilation problems
               (error c)))))
    .elf-truename))

(defun make-arduino-uno (firmware-path &optional (firmware-type :sketch))
  "Make an arduino uno core. It's recommended to use this instead
of (make-instance 'arduino-uno-core). By default, expects a path to a .ino or
sketch directory containing a .ino with the same name as the directory. Will
compile the file using arduino-cli, so make sure it's installed!"
  (declare ((or pathname string) firmware-path))
  (let ((firmware-truename (truename firmware-path)))
    (ecase firmware-type
      ;; path to a sketch directory or file
      (:sketch
       (setq firmware-truename (maybe-compile-sketch firmware-path "uno")))
      ;; an elf file; just pass it on
      (:elf))

    (make-instance 'arduino-uno-core
                   :firmware-path firmware-truename
                   :frequency 16000000
                   :vcc 5
                   :mcu :atmega328p)))

(setf (fdefinition 'make-arduino-nano) #'make-arduino-uno)
