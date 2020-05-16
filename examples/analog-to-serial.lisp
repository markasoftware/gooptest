(in-package #:gooptest/examples)

(defun analog-to-serial ()
  "Test that the analog voltage gets sent over serial UART."

  (runsuite (:name "Analog to Serial"
             :core (make-arduino-uno (find-sketch "analog-to-serial"))
             :setup (uart-start))

    (runtest "Sends Hello World on boot"
      (assert
       (until-uart "Hello World!"
                   :stop '(75 :milliseconds)
                   ;; poll once per millisecond
                   :skip '(1 :millisecond))))

    (runtest "Sends Hello World on boot, tested differently"
      (cycles 49 :ms)
      (assert (zerop (length (uart-string))))
      (cycles 10 :ms)
      (assert (string= (uart-string) "Hello World!")))

    (runtest "Sends one byte"
      (cycles 70 :ms)
      (setf (pin :a0) 2.5)
      ;; until-uart accepts strings, characters, unsigned byte vectors, and
      ;; unsigned bytes!
      (assert
       (until-uart (ash (floor (* 2.5 1023) 5) -2)
                   :stop '(5 :ms)
                   :skip '(100 :us))))

    (runtest "Sends multiple bytes"
      (cycles 70 :ms)

      (setf (pin :a0) 2.5)
      (assert
       (until-uart (ash (floor (* 2.5 1023) 5) -2)
                   :stop '(5 :ms)
                   :skip '(100 :us)))

      (setf (pin :a0) 1.0)
      (assert
       (until-uart (ash (floor 1023 5) -2)
                   :stop '(5 :ms)
                   :skip '(100 :us))))

    ))
