(in-package #:gooptest/examples)

(defun serial-spongebob ()
  "Test that our text gets spongebobified, and explore some nuances of uart
simulation"

  (runsuite (:name "Serial Spongebob"
             :core (make-arduino-uno (find-sketch "serial-spongebob"))
             :setup (uart-start))

    (runtest "Basic Spongebobifying"
      ;; fun fact: this used to be 1000, but the arduino serial can't start up
      ;; in 1000 cycles, so the g of gooptest below was missing.
      (cycles 10000)
      (uart-send "gooptest
")
      (assert (until-uart "gOoPtEsT")))

    (runtest "Advanced Spongebobifying"
      (cycles 10000)
      (uart-send "gooptest
")
      (assert (until-uart "gOoPtEsT"))
      (uart-send "junit
")
      (assert (until-uart "jUnIt")))

    (runtest "Buffer Overflow"
      (let ((lyrics "(till I come back to your side)
We'll forget the tears we've cried
"))

        (cycles 10000)
        (uart-send "wait
")

        ;; this should pass, because the 250ms timer will expire partway through
        ;; the write and let the arduino unload the buffer. At a baud rate of
        ;; 9600 we are doing almost 1 byte/ms (each byte is 10 including start
        ;; and stop). Note that the send works fine even though the arduino is
        ;; listening at a different baud rate than we are sending at; this is
        ;; because the baudrate only matters at the electrical level. TODO:
        ;; throw an error when baud rates mismatch?
        (cycles 200 :ms)
        (assert (uart-send lyrics :baudrate 9600))

        (uart-send "wait
")

        ;; this should fail; at the high baudrate, all bytes get through before
        ;; the 250ms. This would still fail without the (cycles 200 :ms)
        (cycles 200 :ms)
        (assert (not (uart-send lyrics :baudrate 38400)))))))
