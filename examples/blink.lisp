(in-package #:gooptest/examples)

(defun blink ()
  "Test that a blinking LED on the Arduino UNO actually blinks."
  ;; light starts off, turns on after .5s, then turns off after .5s, etc.

  (gooptest:runsuite (:name "Blink Tests"
                      :core-setup (make-arduino-uno (find-sketch "blink")))

    (gooptest:runtest "Light starts in the off state."
     ;; Wait 100 CPU cycles.
     (cycles 1000)
     (assert (eq :low (pin 13))))

    (gooptest:runtest "Light turns on after a little while."
      (cycles 510 :ms)
      (assert (eq :high (pin 13))))
    
    (gooptest:runtest "Light blinks."
     ;; The loop below expects to be started with 400 ms (nominal) until the
     ;; next blink.
     (cycles 100 :ms)

     ;; Test 10 blinks on and off
     (dotimes (i 10)

       ;; (format t "loop ~A~%" i)

       ;; The pin turns off between .39 and .41 seconds later The assertion will
       ;; fail if it turns on before .39 or is still off at .41. Regardless of
       ;; whether the assertion passes, when cycles-between ends, .50 total
       ;; seconds will have elapsed.
       (assert
        (cycles-between (:start '(390 :ms)
                         :stop '(410 :ms)
                         :finally '(500 :ms)
                         :skip '(1 :ms))
          (eq :high (pin 13))))

       ;; Realistically, you could test a blinker pretty well just using
       ;; (cycles), without the fancy (cycles-between).

       (assert
        (cycles-between (:start '(390 :ms)
                         :stop '(410 :ms)
                         :finally '(500 :ms)
                         :skip '(1 :ms))
          (eq :low (pin 13))))))))
