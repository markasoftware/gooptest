;;;; gooptest.lisp

(in-package #:gooptest)

(defvar *core* nil
  "The current core that functions like pin, cycles, etc operate on.")

(defclass core ()
  ((elapsed :initform 0 :accessor core-elapsed)
   (vcc :initform 5 :initarg :vcc :accessor core-vcc)
   (frequency :initarg :frequency :accessor core-frequency
              :initform (error "Core frequency is required"))))

(defgeneric core-one-cycle (c)
  (:documentation "Step forward one cycle. Call the supermethod to increment
  elapsed."))

(defmethod core-one-cycle ((c core))
  (incf (core-elapsed c)))

;; TODO: evaluate whether we really need this method
(defgeneric core-many-cycles (c n)
  (:documentation "Step forward n cycles. When stepping forward many cycles,
  it's faster to implement the loop in C rather than Lisp, so you may want to
  implement this function with FFI."))

(defmethod core-many-cycles ((c core) n)
  (assert (not (minusp n)))
  (dotimes (i n)
    (core-one-cycle c)))

(defgeneric core-pin (c p)
  (:documentation "Return :high, :low, :pull-up, :pull-down, :float. How pins
should be specified is more or less up to the core, depending on how the
manufacturer refers to their pins. All pins should be symbols, and probably
keywords."))

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

Cores can define their own values as necessary.

Respects *core*."
  (assert *core*)
  (core-pin *core* the-pin))

(defun elapsed ()
  "Returns the number of elapsed CPU cycles.

Respects *core*."
  (core-elapsed *core*))

;; A "timespec" is a list with up to one to three elements, or a single number.
;; The first is the number of time units. The second is the time unit. The third
;; is a boolean indicating whether the timespec is absolute (measured from
;; boot). If a single number is used instead of a list, it is the number of
;; relative cycles.

(defun resolve-timespec (timespec into-absolute)
  "Convert the given timespec into a number of cycles.

Respects *core*."
  (destructuring-bind (n &optional (unit :cycle) absolute)
      (ensure-list timespec)
    (let ((cycles
           (if (member unit '(:cycle :cycles)) n
               (floor (* n (core-frequency *core*)
                         (ecase unit
                           ((:ns :nanosecond :nanoseconds) 1/1000000000)
                           ((:us :microsecond :microseconds) 1/1000000)
                           ((:ms :millisecond :milliseconds) 1/1000)
                           ((:s :second :seconds) 1)
                           ((:min :minute :minutes) 60)
                           ((:hr :hour :hours) 3600)))))))
      (cond
        ((eq absolute into-absolute) cycles)
        (absolute (- cycles (core-elapsed *core*)))
        (t (+ cycles (core-elapsed *core*)))))))

(defmacro resolve-timespecs (absolute-specs relative-specs &body body)
  `(let ,(loop for sym in absolute-specs
            collect `(,sym (resolve-timespec ,sym t)))
     (let ,(loop for sym in relative-specs
              collect `(,sym (resolve-timespec ,sym nil)))
       ,@body)))

(defun cycles-rel (n)
  "Run n cycles.

Respects *core*."
  (core-many-cycles *core* n))

(defun cycles-abs (n)
  "Run until n total cycles have elapsed.

Respects *core*."
  (cycles-rel (max 0 (- n (elapsed)))))

(defun cycles (n-or-timespec &optional (unit :cycle) absolute)
  "Move forward in time to a certain timespec. The timespec can either be given
as the first argument, or can be split up among the argument as suggested by
the lambda-list. If the timespec is in the past, does nothing.

Respects *core"
  (assert *core*)
  (cycles-rel
   (max 0 (resolve-timespec
           (etypecase n-or-timespec
             (cons n-or-timespec)
             (number (list n-or-timespec unit absolute)))
           nil))))

(defun until-pin (pin-id state &optional (timeout '(5 :seconds)) (poll 1))
  "Waits until the pin is in the given state. Returns non-nil if the pin is in
the desired state.

Respects *core*"
  (resolve-timespecs (timeout) (poll)
    (loop
       while (<= (+ (elapsed) poll) timeout)
       thereis (eq (pin pin-id) state)
       do (cycles-rel poll))))

(defmacro cycles-between
    ((&key (start 0) stop (skip 1) (finally 0)) &body condition)
  "Runs :start cycles, then run condition every :skip cycles until it returns
non-nil. If it never returns non-nil, stop running it after :stop
cycles (counting from when cycles-between is called) anyway. Returns the
result of the condition or nil if the condition never succeeded. Ensures that
:finally has elapsed before exiting, to ensure a consistent exit state (if you
want).

:start, :stop, :skip, and :finally are timespecs.

Respects *core*."
  (assert (>= stop start))
  (once-only (start stop finally skip)
    `(resolve-timespecs (,start ,stop ,finally) (,skip)
       (assert *core*)
       (cycles-abs ,start)
       ;; TODO: verify off-by-one-errors (i.e, are the correct number of
       ;; cycles always run?)
       (prog1
           (loop
              while (<= (+ (elapsed) ,skip) ,stop)
              thereis (progn ,@condition)
              do (cycles-rel ,skip))
         (cycles-abs ,finally)))))

(defun pin-duty-cycle (p stop &optional (skip 1))
  "Returns the duty cycle of the given pin, as a fraction. Records for the
length given by the timespec. Works for digital output pins; will throw an error
if a pin state other than :high or :low is detected. skip-timespec can be used
to improve performance by specifying how many cycles to let pass between polling
the pin.

Respects *core*"
  (resolve-timespecs (stop) (skip)
    ;; TODO: verify off-by-one errors (i.e, does it stop one poll-timespec
    ;; before timespec? Or after? Does it always run exactly timespec cycles?)
    (loop
       with positive-samples = 0
       for total-samples from 1
       ;; use ecase to throw errors on non-digital values.
       when (ecase (pin p)
              (:high t)
              (:low nil))
       do (incf positive-samples)
       do (cycles-rel skip)
       while (<= (+ (elapsed) skip) stop)
       finally (return (/ positive-samples total-samples)))))

;;; TEST UTILITIES

(defmacro runsuite ((&key setup core-setup teardown name) &body body)
  "Create a Gooptest suite. A suite is a collection of related tests. Setup and
teardown are run before and after every test in the lexical environment of the
body of defsuite. Core-setup is run after setup and its result is dynamically
bound to *core* before every test, for convenience."
  `(let ((suite-name (or ,name "unnamed-suite")))
     ;; These symbols are in the gooptest package, so no need to fear!
     (flet ((suite-setup () ,setup)
            (suite-core-setup () ,core-setup)
            (suite-teardown () ,teardown))
       ,@body)))

(defmacro runtest (name &body body)
  "Run a test, respecting the current suite and catching any errors. Name is
optional, like the documentation in a defun; it is part of the body if it is not
a compile-time string."
  (unless (stringp name)
    (setq name "unnamed-test"
          ;; if both name and body are nil, body shoud not be '(nil . nil)
          body (when (or name body) (cons name body))))
  `(if suite-name
       (progn
         (suite-setup)
         (with-core (suite-core-setup)
           ,@body)
         (suite-teardown))
       (progn ,@body)))

(defun assert-pin (state pin-sym)
  (assert (eq state (pin pin-sym))))
