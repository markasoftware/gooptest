(in-package #:gooptest/examples)

;; Convenience function, using reasonable parameters for stop and skip.
(defun duty-cycle ()
  (pin-duty-cycle 5 '(100 :ms) '(50 :us)))

(defun analog-to-pwm ()
  "Test that a PWM output's duty cycle is proportional to an analog signal."
  ;; Recall that the Arduino Uno has <1KHz PWM, so to get an accurate reading of
  ;; the duty cycle, we have to poll over quite a long period.

  (runsuite (:name "Analog to PWM"
             :core-setup (make-arduino-uno (find-sketch "analog-to-pwm")))

    (runtest "Stays off by default"
      (cycles 1000)
      (assert (zerop (duty-cycle))))

    (runtest "50% duty cycle"
      (setf (pin :a0) 2.5)
      (cycles 1000)
      (assert (< 0.45 (duty-cycle) 0.55)))))
