;;;; gooptest.lisp

(in-package #:gooptest)

(defvar *core* nil
  "The current core that functions like pin, cycles, etc operate on. Usually set
  using with-core.")

(defclass core ()
  ((elapsed :initform 0 :accessor core-elapsed)
   (vcc :initform 5 :initarg :vcc :accessor core-vcc)
   (frequency :initarg :frequency :accessor core-frequency)))

(defmethod core-inc-elapsed ((c core) n)
  "Increment the elapsed cycle counter by n. Cores should call this method as
  part of core-one-cycle and core-many-cycles (if they override it)."
  (incf (core-elapsed c) n))

(defgeneric core-one-cycle (c)
  (:documentation "Step forward one cycle."))

;; TODO: evaluate whether we really need this method
(defgeneric core-many-cycles (c n)
  (:documentation "Step forward n cycles. When stepping forward many cycles,
  it's faster to implement the loop in C rather than Lisp, so you may want to
  implement this function with FFI."))

(defmethod core-cycles ((c core) n)
  (dotimes (i n)
    (core-one-cycle c)))

(defgeneric core-pin (c p)
  (:documentation "Return :high, :low, :pull-up, :pull-down, :float, or some
  pin. How pins should be specified is more or less up to the core, depending on
  how the manufacturer refers to their pins. All pins should be symbols."))

(defgeneric core-set-pin-digital (c p high)
  (:documentation "Set a digital input at a pin. Pin should be in the same
  format as (core-pin). High represents a high voltage. An error may be thrown
  if the pin is in output mode, but this behavior is not guaranteed."))

(defgeneric core-set-pin-analog (c p voltage)
  (:documentation "Set an analog input voltage at a pin. Pin should be in the
  same format as (core-pin). The voltage represents the absolute voltage at the
  input. It may be useful to compare this to (core-vcc). An error may bethrown
  if the pin is in output mode, but this behavior is not guaranteed."))

(deftype pin-output ()
  "What an mcu might set a pin to"
  '(member :float :high :low :pull-up :pull-down))

(defstruct pin
  "Represents a sigle pin, including:
What state the mcu has set the pin to.
What digital value the external circuit has applied to the pin.
What analog value the external circuit has applied to the pin."
  (output :float :type pin-output)
  (digital-input nil :type boolean)
  (analog-input 0.0 :type real))

(defmacro with-core (the-core &body body)
  `(let ((*core* ,the-core))
     ,@body))

(defun pin (the-pin)
  "Return the status of the given pin. The format of the argument is determined
  by the core. Respects *core*. Valid values include:

    :high     (set to digital high)
    :low      (set to digital low)
    :pull-up  (configured as input, with pullup resistor)
    :pull-down
    :float    (configured as input, no pullup resistor)

  Cores can define their own values as necessary."
  (assert *core*)
  (core-pin *core* the-pin))

(defun time-cycles (n &optional (unit :cycle) absolute demand-exact)
  "Convert n of unit into a number of clock cycles. Unit can be one of :cycle,
:ns, :us, :ms, :s, :min, :hr. Rounds down to the next CPU cycle, unless
demand-exact is set, in which case an error is raised. If absolute is set, it
will translate from an absolute time (since the simulation was started) into a
relative time (from the current state of the core).

Respects *core*."
  (assert *core*)
  (if (member unit '(:cycle :cycles))
      n
      (let ((cycles-fractional
             (* n
                (core-frequency *core*)
                (ecase unit
                  ((:ns :nanosecond :nanoseconds) 1/1000000000)
                  ((:us :microsecond :microseconds) 1/1000000)
                  ((:ms :millisecond :milliseconds) 1/1000)
                  ((:s :second :seconds) 1)
                  ((:min :minute :minutes) 60)
                  ((:hr :hour :hours) 3600)))))
        ;; TODO: a proper error
        (when demand-exact
          (assert (integerp cycles-fractional)))
        (if absolute
            (progn
              (assert (>= cycles-fractional (core-elapsed *core*)))
              (- (floor cycles-fractional) (core-elapsed *core*)))
            (values (floor cycles-fractional))))))

(defun cycles (n &optional (unit :cycle))
  "Move forward n cycles.

Respects *core"
  (assert *core*)
  (let ((c (time-cycles n unit)))
    (if (= c 1)
        (core-one-cycle *core*)
        (core-many-cycles *core* c))))

(defmacro resolve-timespecs (syms &body body)
  `(let ,(loop for sym in syms
            collect `(,sym (apply #'time-cycles (ensure-list ,sym))))
     ,@body))

(defmacro cycles-between
    ((&key (start 0) stop (finally 0) (skip 1)) &body condition)
  "Executes :start cycles, then run condition every :skip cycles until it
  returns non-nil. If it never returns non-nil, stop running it after :stop
  cycles (counting from when cycles-between is called) anyway. Regardless of if
  condition every returned non-nil, make sure at least :finally total cycles
  have been run before returning; this ensures the core is left in a consistent
  state every time.

  :start, :stop, :finally, and :skip may either be numbers, representing clock
  cycles, or they may be timespecs, eg (10 :us)

  Respects *core*"
  ;; TODO: look into how the hell documentation indentation works. And whether
  ;; we should use a trailing period!

  (assert (>= stop start))
  (with-gensyms (elapsed condition-result)
    (once-only (start stop finally skip)
      `(resolve-timespecs (,start ,stop ,finally ,skip)
         (assert *core*)
         (cycles ,start)
         ;; TODO: verify off-by-one-errors (i.e, are the correct number of
         ;; cycles always run?)
         (loop
            for ,elapsed from ,start to ,elapsed by ,skip
            and ,condition-result = (progn ,@condition)
            while (<= ,elapsed ,stop)
            until ,condition-result
            do (cycles ,skip)
            finally (progn
                      (cycles (max 0 (- ,finally ,elapsed)))
                      (return ,condition-result)))))))

(defun pin-duty-cycle (p timespec &optional (poll-timespec 1))
  "Returns the duty cycle of the given pin, as a fraction. Records for the
length given by the timespec. Works for digital output pins; will throw an
error if a pin state other than :high or :low is detected. skip-timespec can
be used to improve performance by specifying how many cycles to let pass
between polling the pin.

Respects *core*"
  (resolve-timespecs (timespec poll-timespec)
    ;; TODO: verify off-by-one errors (i.e, does it stop one poll-timespec
    ;; before timespec? Or after? Does it always run exactly timespec cycles?)
    (loop
       with positive-samples = 0
       for total-samples from 1
       and elapsed from 0 to timespec by poll-timespec
       ;; use ecase to throw errors on non-digital values.
       when (ecase (pin p)
              (:high t)
              (:low nil))
       do (incf positive-samples)
       do (cycles poll-timespec)
       finally (return (/ positive-samples total-samples)))))
