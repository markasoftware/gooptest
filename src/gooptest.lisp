;;;; gooptest.lisp

(in-package #:gooptest)

(defvar *core* nil
  "The current core that functions like pin, cycles, etc operate on.")

(defclass core ()
  ((elapsed :initform 0 :accessor core-elapsed :type integer)
   (vcc :initform 5 :initarg :vcc :accessor core-vcc :type number)
   (frequency :initarg :frequency :accessor core-frequency
              :initform (error "Core frequency is required")
              :type integer)
   ))

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

(defgeneric core-uart-default-channel (c)
  (:documentation "Return the a reasonable default uart-channel that can be
passed as the last argument to the core-uart-* functions. If not implemented,
functions like uart-start will error out if not passed a uart-channel
argument."))

(defgeneric core-uart-start (c uart-channel)
  (:documentation "Start listening and recording uart data on the given
channel (or some reasonable default). It is valid for a core to listen to uart
passively, without this function being called. uart-channel = nil is equivalet
to the default, even if it's provided."))

(defgeneric core-uart-stop (c uart-channel)
  (:documentation "May not do anything. You do /not/ need to call this before
throwing away a core."))

(defmethod core-uart-stop ((c core) uart-channel)
  "Noop. Override if you care."
  (declare (ignore uart-channel)))

(defgeneric core-uart-data (c uart-channel)
  (:documentation "Retrieve, as a vector of unsigned bytes, all uart data that
has been sent so far on the given channel."))

(defgeneric core-uart-send (c byte uart-channel)
  (:documentation "Send the given byte to the mcu over uart, immediately (no
buffering on our side). Returns nil if the byte could not be received by the
mcu (probably due to a full buffer, but not necessarily)."))

(defun uart-string (&optional channel)
  "Return all data sent over UART, as a string. Uses babel's default encoding;
call (babel:octets-to-string) manually to override.

Respects *core*"
  (babel:octets-to-string (uart-data channel)))

(deftype pin-output ()
  "What an mcu might set a pin to."
  '(member :float :high :low :pull-up :pull-down))

(defmacro with-core (the-core &body body)
  `(let ((*core* ,the-core))
     ,@body))

(defmacro defcorewrapper (new-fun old-fun lambda-list &optional last-arg-getter)
  "Creates a *core*-respecting wrapper for a CLOS method on core."
  `(defun ,new-fun ,lambda-list
     ,(concatenate 'string
                   (documentation old-fun 'function)
                   "

Respects *core*.")
     (assert *core*)
     ,(if last-arg-getter
          `(,old-fun *core* ,@(loop for el in lambda-list
                                 until (eq '&optional el)
                                 collect el)
                     (or ,(lastcar lambda-list) (,last-arg-getter *core*)))
          `(,old-fun *core* ,@lambda-list))))

(defcorewrapper pin core-pin (pin-designator))
(defcorewrapper set-pin-digital core-set-pin-digital (pin-designator high))
(defcorewrapper set-pin-analog core-set-pin-analog (pin-designator voltage))
(defcorewrapper elapsed core-elapsed ())
(defcorewrapper uart-start core-uart-start (&optional uart-channel)
                core-uart-default-channel)
(defcorewrapper uart-stop core-uart-stop (&optional uart-channel)
                core-uart-default-channel)
(defcorewrapper uart-data core-uart-data (&optional uart-channel)
                core-uart-default-channel)
(defcorewrapper uart-send-byte core-uart-send (byte &optional uart-channel)
                core-uart-default-channel)

(defsetf pin (p) (new-state)
  "Set the pin to a certain state. If new-state is a number, set to that
voltage (analog). If new-state is nil or :low, set to digital low. If new-state
is any other non-nil value (:high recommended), set to digital high. Setting the
digital state will not change the analog state, and vice versa."
  `(progn
     (cond
       ((numberp ,new-state) (set-pin-analog ,p ,new-state))
       ((or (not ,new-state) (eq :low ,new-state)) (set-pin-digital ,p nil))
       (t (set-pin-digital ,p t)))
     ,new-state))

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
                           ((:m :minute :minutes) 60)
                           ((:h :hour :hours) 3600)))))))
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

(defun until-pin (pin-id state &optional (timeout '(1 :second)) (poll 100))
  "Waits until the pin is in the given state. Returns non-nil if the pin is in
the desired state.

Respects *core*"
  (resolve-timespecs (timeout) (poll)
    (loop
       while (<= (+ (elapsed) poll) timeout)
       thereis (eq (pin pin-id) state)
       do (cycles-rel poll))))

(defmacro cycles-between
    ((&key (start 0) stop (skip 100) (finally 0)) &body condition)
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

;; (defun pin-duty-cycle (p stop &optional (skip 1))
;;   "Returns the duty cycle of the given pin, as a fraction. Records for the
;; length given by the timespec. Works for digital output pins; will throw an error
;; if a pin state other than :high or :low is detected. skip-timespec can be used
;; to improve performance by specifying how many cycles to let pass between polling
;; the pin.

;; Respects *core*"
;;   (resolve-timespecs (stop) (skip)
;;     ;; TODO: verify off-by-one errors (i.e, does it stop one poll-timespec
;;     ;; before timespec? Or after? Does it always run exactly timespec cycles?)
;;     (loop
;;        with positive-samples = 0
;;        for total-samples from 1
;;        ;; use ecase to throw errors on non-digital values.
;;        when (ecase (pin p)
;;               (:high t)
;;               (:low nil))
;;        do (incf positive-samples)
;;        while (<= (+ (elapsed) skip) stop)
;;        do (cycles-rel skip)
;;        finally (return (/ positive-samples total-samples)))))

;; TODO: normalize the parameters. I think we should call start, stop, skip,
;; finally. (i.e, rename timeout -> stop on other methods). Whenever we do this,
;; also update the etypecase below to be more recursive!
(defun until-uart (text &key serial-port (stop '(1 :s)) (skip 100) (finally 0))
  "Runs until the given text is sent over the serial port. Returns non-nil if
that text was found before the timeout. Ignores text that was already sent
before (until-uart) was called. Will poll at intervals indicated by skip.
Handles strings, characters, numbers/unsigned bytes, and vectors of unsigned
bytes as text.

Respects *core*."
  ;; TODO: do we need to store string length separately from binary length? I
  ;; want it all to be ascii but who knows.
  (let ((start (length (uart-data))))
    (cycles-between (:start 0 :stop stop :skip skip :finally finally)
      (etypecase text
        (string (search text (uart-string serial-port)
                        :from-end t :start2 start))

        (character (position text (uart-string serial-port)
                             :from-end t :start start))

        ((unsigned-byte 8) (position text (uart-data serial-port)
                                     :from-end t :start start))

        ((vector (unsigned-byte 8)) (search text (uart-data serial-port)
                                            :from-end t :start2 start))))))

(defvar *uart-baudrate* 115200
  "The default baudrate for (uart-send). Note that if the frequency is not
totally divisible by the byterate, you should decrease it to account for the
error, otherwise you will get missing bytes.")
(defvar *uart-byte-size* 10
  "The default byte size for (uart-send)")

(defun uart-send
    (data
     &key
       (channel (core-uart-default-channel *core*))
       (baudrate *uart-baudrate*)
       (byte-size *uart-byte-size*)
       (finally t)
     &aux (cycles-per-byte (ceiling (core-frequency *core*)
                                    (/ baudrate byte-size)))
       buffer-overflowed-p)
  "Send the given data, which should be a string, byte array, byte, or
character, over the given uart channel (or the default). To control character
encoding, use a byte array. Returns non-nil if all data were sent without
overflowing any buffers on the mcu. Uses the baud rate and byte size to control
the delay between characters. If finally is non-nil (the default), will wait at
the end of transmission the equivalent of one characters length, so that it is
safe to call (uart-send) multiple times in a row.

Respects *core*"
  (setf data
        (etypecase data
          ((or string character) (babel:string-to-octets (string data)))
          ((unsigned-byte 8)
           (return-from uart-send (uart-send-byte data channel)))
          ((vector (unsigned-byte 8)) data))) 
  (loop
     for byte across data
     for i from 0
     unless (uart-send-byte byte channel)
     do (setf buffer-overflowed-p t)
     ;; fuck fenceposts dude
     when (or finally (< i (1- (length data))))
     do (cycles-rel cycles-per-byte))
  (not buffer-overflowed-p))

;;; TEST UTILITIES

(defvar *suite* nil
  "The symbol designating the current test suite.")

(defmacro in-suite (sym)
  `(setq *suite* ',sym))

(defmacro defsuite (name &key setup (core *core*) teardown)
  "Expands into a form that defines a test suite with the given name. Before
each test in the suite, *core* is set to the result of :core, then :setup is
run. :teardown is run after each test."
  (declare (symbol name))
  ;; haters will say it's photoshop
  `(setf (get ',name 'suite) `(,',setup ,',core ,',teardown)))

(defmacro runtest (name &body body)
  "Run a test, respecting the current suite and catching any errors."
  `(if *suite*
       (destructuring-bind (setup core teardown) (get *suite* 'suite)
         (format t "~A/~A RUN..." *suite* ,name)
         (with-core (eval core)
           (eval setup)
           ,@body
           (eval teardown))
         (format t "PASS~%"))
       (progn
         (format t "~A RUN..." ,name)
         ,@body
         (format t "PASS~%"))))

(defun assert-pin (state pin-sym)
  (assert (eq state (pin pin-sym))))
