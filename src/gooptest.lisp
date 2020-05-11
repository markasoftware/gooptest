;;;; gooptest.lisp

(in-package #:gooptest)

(defvar *core* nil
  "The current core that functions like pin, cycles, etc operate on.")

(defclass core ()
  ((elapsed :initform 0 :accessor core-elapsed :type fixnum)
   (vcc :initform 5 :initarg :vcc :accessor core-vcc)
   (frequency :initarg :frequency :accessor core-frequency
              :initform (error "Core frequency is required"))))

(setf (documentation 'core-elapsed 'function)
      "The number of clock cycles executed.")

(defgeneric core-one-cycle (c)
  (:documentation "Step forward the minimum number of cycles. Make sure to
increment core-elapsed appropriately! Some simulators rarely do a true single
cycle, and instead batch together multi-cycle instructions that can be executed
very quickly on desktop (eg, 32-bit operations on an 8-bit microcontroller)."))

(defgeneric core-many-cycles (c n)
  (:documentation "Step forward n cycles."))

(defmethod core-many-cycles ((c core) n)
  (declare (fixnum n))
  (assert (not (minusp n)))
  (do ((stop-abs (+ n (core-elapsed c)))) ((>= (core-elapsed c) stop-abs))
    (core-one-cycle c)))

(defgeneric core-pin (c p)
  (:documentation
   "Return the status of the given pin. The format of the argument is determined
by the core. Respects *core*. Valid values include:

:high     (set to digital high)
:low      (set to digital low)
:pull-up  (configured as input, with pullup resistor)
:pull-down
:float    (configured as input, no pullup resistor)

Cores can define their own values as necessary."))

(defgeneric core-set-pin-digital (c p high)
  (:documentation "Set a digital input at a pin. Pin should be in the same
format as (pin). High should be non-nil to set to a high voltage, or nil for
low."))

(defgeneric core-set-pin-analog (c p voltage)
  (:documentation "Set an analog input voltage at a pin. Pin should be in the
same format as (pin). The voltage represents the absolute voltage at the
input."))

(deftype pin-output ()
  "What an mcu might set a pin to"
  '(member :float :high :low :pull-up :pull-down))

(defmacro with-core (the-core &body body)
  `(let ((*core* ,the-core))
     ,@body))

(defmacro defcorewrapper (new-fun old-fun lambda-list)
  "Creates a *core*-respecting wrapper for a CLOS method on core."
  `(defun ,new-fun ,lambda-list
     ,(documentation old-fun 'function)
     (assert *core*)
     (,old-fun *core* ,@lambda-list)))

(defcorewrapper pin core-pin (pin-designator))
(defcorewrapper set-pin-digital core-set-pin-digital (pin-designator high))
(defcorewrapper set-pin-analog core-set-pin-analog (pin-designator voltage))
(defcorewrapper elapsed core-elapsed ())

(defsetf pin (p) (new-state)
  "Set the pin to a certain state. If new-state is a number, set to that
voltage (analog). If new-state is nil or :low, set to digital low. If new-state
is any other non-nil value (:high recommended), set to digital high. Setting the
digital state will not change the analog state, and vice versa."
  `(cond
     ((numberp ,new-state) (set-pin-analog ,p ,new-state))
     ((or (not ,new-state) (eq :low ,new-state)) (set-pin-digital ,p nil))
     (t (set-pin-digital ,p t))))

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

(defmacro runsuite ((&key setup (core-setup *core*) teardown name) &body body)
  "Create a Gooptest suite. A suite is a collection of related tests. Setup and
teardown are run before and after every test in the lexical environment of the
body of defsuite. Core-setup is run after setup and its result is dynamically
bound to *core* before every test, for convenience."
  ;; These symbols are in the gooptest package, so no need to fear!
  `(macrolet ((runtest (name &body body)
                `(progn
                   (format t "    ~A RUN..." ,name)
                   ;; I thought I would take this opportunity to really learn
                   ;; and understand double backquote syntax. But instead, I
                   ;; just fucked around with different combinations of quotes
                   ;; and commas until it worked. My best guess: The rightmost
                   ;; comma belongs to the outermost backquote. This gets
                   ;; expanded to ,'(make-arduino-uno) for example.
                   ,',setup
                   (with-core ,',core-setup
                     ,@body)
                   ,',teardown
                   (format t "PASS~%"))))
     (format t "SUITE ~A BEGIN~%" ,name)
     ,@body
     (format t "SUITE ~A END~%" ,name)))

(defmacro runtest (name &body body)
  "Run a test, respecting the current suite and catching any errors."
  `(progn
     (format t "~A RUN..." ,name)
     ,@body
     (format t "PASS~%")))

(defun assert-pin (state pin-sym)
  (assert (eq state (pin pin-sym))))
