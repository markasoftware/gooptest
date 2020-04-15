;;;; gooptest.lisp

(in-package #:gooptest)

(defvar *core* nil
  "The current core that functions like pin, cycles, etc operate on. Usually set
  using with-core.")

(defclass core ()
  ((elapsed :initform 0 :accessor core-elapsed)))

(defmethod core-inc-elapsed ((c core) n)
  "Increment the elapsed cycle counter by n. Cores should call this method as
  part of core-one-cycle and core-many-cycles (if they override it)."
  (incf (slot-value c 'elapsed) n))

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

(defgeneric core-frequency (c)
  (:documentation "Return the frequency, in cycles per second"))

(defgeneric core-pin (c p)
  (:documentation "Return :high, :low, :pull-up, :pull-down, :float, or some
  pin. How pins should be specified is more or less up to the core, depending on
  how the manufacturer refers to their pins. All pins should be symbols."))

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

(defun cycles (n &optional unit)
  "Move forward n cycles.

Respects *core"
  (assert *core*)
  (core-cycles *core* (time-cycles n (or unit :cycle))))

(defmacro once-only-timespec (syms &body body)
  `(once-only (,@(loop for sym in syms
                   collect `(,sym (time-cycles ,@(ensure-list sym)))))
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
    (once-only-timespec (start stop finally skip)
      `(progn
         (assert *core*)
         (cycles ,start)
         (loop
            for ,elapsed = ,start then (+ ,elapsed ,skip)
            and ,condition-result = (progn ,@condition)
            while (<= ,elapsed ,stop)
            until ,condition-result
            do (cycles ,skip)
            finally (progn
                      (cycles (max 0 (- ,finally ,elapsed)))
                      (return ,condition-result)))))))
