(in-package #:gooptest/examples)

(defsuite at-least-two
    :core (make-arduino-uno (find-sketch "at-least-two")))

(defun at-least-two ()
  "Test that an LED turns on when at least two of three buttons are depressed."

  (load-foreign-libraries)

  (in-suite at-least-two)

  (runtest "Light starts in the off state"
    (cycles 1000)
    (assert-pin :low 13))

  (runtest "One button is not enough"
    (setf (pin 4) :high)
    (cycles 1000)
    (assert-pin :low 13))

  (runtest "Other button is not enough either"
    (setf (pin 6) :high)
    (cycles 1000)
    (assert-pin :low 13))

  (runtest "Two and three pins are enough"
    (setf (pin 4) :high)
    (setf (pin 5) :high)
    (cycles 1000)
    (assert-pin :high 13)

    (setf (pin 6) :high)
    (cycles 1000)
    (assert-pin :high 13))

  (runtest "Turns off again when buttons released"
    (setf (pin 4) :high)
    (setf (pin 5) :high)
    (cycles 1000)
    (setf (pin 4) :low)
    (cycles 1000)
    (assert-pin :low 13)))
