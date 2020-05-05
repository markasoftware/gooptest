;;;; gooptest.lisp

(in-package #:gooptest)

(defvar *core* nil
  "The current core that functions like pin, cycles, etc operate on. Usually set
  using with-core.")

(defclass core ()
  ((elapsed :initform 0 :accessor core-elapsed)
   (vcc :initform 5 :initarg :vcc :accessor core-vcc)
   (frequency :initarg :frequency :accessor core-frequency
              :initform (error "Core frequency is required"))))

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

  Cores can define their own values as necessary."
  (assert *core*)
  (core-pin *core* the-pin))

;; A "timespec" is a list with up to one to three elements, or a single number.
;; The first is the number of time units. The second is the time unit. The third
;; is a boolean indicating whether the timespec is absolute (measured from
;; boot). If a single number is used instead of a list, it is the number of
;; relative cycles.

(defmacro with-timespec (cycles-var absolute-var timespec &body body)
  (with-gensyms (n unit)
    (once-only (timespec)
      `(destructuring-bind (,n &optional (,unit :cycle) ,absolute-var)
           (ensure-list ,timespec)
         (let ((,cycles-var
                (if (member ,unit '(:cycle :cycles)) ,n
                    (floor (* ,n (core-frequency *core*)
                              (ecase ,unit
                                ((:ns :nanosecond :nanoseconds) 1/1000000000)
                                ((:us :microsecond :microseconds) 1/1000000)
                                ((:ms :millisecond :milliseconds) 1/1000)
                                ((:s :second :seconds) 1)
                                ((:min :minute :minutes) 60)
                                ((:hr :hour :hours) 3600)))))))
           ,@body)))))

(defun time-cycles (timespec into-absolute)
  "Convert the given timespec into a number of cycles. If into-absolute is
  non-nil, return a number of cycles between boot and the timespec. If nil,
  return a number of cycles from the current state to the timespec, even if the
  timespec itself is absolute.

Respects *core*."
  (assert *core*)
  (with-timespec n absolute timespec
    (cond
      ((eq absolute into-absolute) n)
      (absolute (- n (core-elapsed *core*)))
      (t (+ n (core-elapsed *core*))))))

(defun timespec+ (t1 &rest ts)
  (if (null ts) t1
      (with-timespec n1 abs1 t1
        (with-timespec ns abss (apply #'timespec+ ts)
          (list (+ n1 ns) :cycle (or abs1 abss))))))

(defun check-timespec (timespec)
  "Returns :present, :past, or :future, depending on whether the timespec
represents the present, past, or future.

Respects *core*"
  (let ((abs-cycles (time-cycles timespec t)))
    (cond
      ((> abs-cycles (core-elapsed *core*)) :future)
      ((< abs-cycles (core-elapsed *core*)) :past)
      (t :present))))

(defun future-p (timespec)
  "Check if a timespec is in the future"
  (eq :future (check-timespec timespec)))

(defun resolve-timespec (timespec)
  "Convert the timespec, which may be relative, into an absolute timespec."
  (list (time-cycles timespec t) :cycle t))

(defun cycles (n-or-timespec &optional (unit :cycle) absolute)
  "Move forward in time to a certain timespec. The timespec can either be given
   as the first argument, or can be split up among the argument as suggested by
   the lambda-list. If the timespec is in the past, does nothing.

Respects *core"
  (assert *core*)
  (core-many-cycles *core*
                    (max 0 (time-cycles
                            (etypecase n-or-timespec
                              (cons n-or-timespec)
                              (number (list n-or-timespec unit absolute)))
                            nil))))

(defmacro resolve-timespecs (syms &body body)
  `(let ,(loop for sym in syms
            collect `(,sym (resolve-timespec ,sym)))
     ,@body))

(defun until-pin (pin state &optional (timeout '(5 :seconds)) (poll 1))
  "Waits until the pin is in the given state.

Respects *core*"
  (resolve-timespecs timeout
    ))

(defmacro cycles-between
    ((&key (start 0) stop (skip 1)) &body condition)
  "Runs :start cycles, then run condition every :skip cycles until it returns
  non-nil. If it never returns non-nil, stop running it after :stop
  cycles (counting from when cycles-between is called) anyway. Returns the
  result of the condition or nil if the condition never succeeded.

  :start, :stop, and :skip are timespecs.

  Respects *core*"
  ;; TODO: look into how the hell documentation indentation works. And whether
  ;; we should use a trailing period!

  (assert (>= stop start))
  (with-gensyms (elapsed condition-result)
    (once-only (start stop finally skip)
      `(resolve-timespecs (,start ,stop ,finally)
         (assert *core*)
         (cycles ,start)
         ;; TODO: verify off-by-one-errors (i.e, are the correct number of
         ;; cycles always run?)
         (loop
            for ,condition-result = (progn ,@condition)
            while (future-p (timespec+ (core-elapsed *core*) ,skip))
            until ,condition-result
            do (cycles ,skip)
            finally (progn
                      (cycles ,stop)
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
