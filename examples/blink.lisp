(in-package #:gooptest-examples)

(defun blink ()
  "Test that a blinking LED on the Arduino UNO actually blinks."

  (with-core (make-arduino-uno)

    (test "Light starts in the off state."
     ;; Wait 10,000 CPU cycles.
     (cycles 10000)
     (assert (not (pin :d13)))
     (assert (eq (pin-mode :d13) :output)))
    
    (test "Light blinks"
     ;; The loop below expects to be started with 400 ms (nominal) until the
     ;; next blink. This could have been written: (cycles (time-cycles 100
     ;; :milli-second))
     (cycles 100 :ms)

     ;; Test 10 blinks on and off
     (dotimes (i 10)

       ;; The pin turns off between .39 and .41 seconds later The assertion will
       ;; fail if it turns on before .39 or is still off at .41. Regardless of
       ;; whether the assertion passes, when cycles-between ends, .50 total
       ;; seconds will have elapsed.
       (assert
        (cycles-between (:start (390 :ms)
                         :stop (410 :ms)
                         :finally (500 :ms))
         (pin :d13)))

       (assert
        (cycles-between (:start (390 :ms)
                         :stop (410 :ms)
                         :finally (500 :ms))
         (not (pin :d13))))))))
