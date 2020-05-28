(in-package #:gooptest/examples)

(defsuite serial-spongebob
    :core (make-arduino-uno (find-sketch "serial-spongebob"))
    :setup (uart-start))

(defun serial-spongebob ()
  "Test that our text gets spongebobified, and explore some nuances of uart
simulation"

  ;; Two compounding issues: simavr is bugged and thinks 8N1 has 11 bits per
  ;; byte instead of 10, and 115200 has a -3.5% error at 16MHz.
  (let ((*suite* 'serial-spongebob)
        (*uart-baudrate* (floor (* 0.96 115200)))
        (*uart-byte-size* 11))

    (runtest "Basic Spongebobifying"
      ;; fun fact: this used to be 1000, but the arduino serial can't start up
      ;; in 1000 cycles, so the g of gooptest below was missing.
      (cycles 10000)
      (assert (uart-send "gooptest
"))
      (assert (until-uart "gOoPtEsT")))

    (runtest "Advanced Spongebobifying"
      (cycles 10000)
      (uart-send "gooptest
" :finally t)
      ;; If we didn't use :finally t, we would need an extra (cycles 1500) or so
      ;; here to manually add the delay between adjacent (uart-send) calls.
      (uart-send "junit
")
      (assert (until-uart "jUnIt")))

    (runtest "Buffer Overflow"
      (let* ((lyrics "(till I come back to your side) We'll forget the tears we've cried
")
             (spongey-lyrics
              (coerce
               (loop
                  for i from 0
                  for char across lyrics
                  unless (char= char #\Newline)
                  collect (if (zerop (mod i 2))
                              (char-downcase char)
                              (char-upcase char)))
               'string)))

        (cycles 10000)
        (uart-send "wait
")

        ;; this should pass, because the 250ms timer will expire partway through
        ;; the write and let the arduino unload the buffer. At a baud rate of
        ;; 9600 we are doing almost 1 byte/ms (each byte is 10 including start
        ;; and stop). Note that the send works fine even though the arduino is
        ;; listening at a different baud rate than we are sending at; this is
        ;; because the baudrate only matters at the electrical level.
        (cycles 200 :ms)
        (uart-send lyrics :baudrate 9600 :finally nil)
        (assert (until-uart spongey-lyrics))

        (uart-send "wait
")

        ;; this should fail; at the high baudrate, all bytes get through before
        ;; the 250ms. This would still fail without the (cycles 200 :ms)
        (cycles 200 :ms)
        (uart-send lyrics)
        (assert (not (until-uart spongey-lyrics)))
        ;; Inspect (uart-string) here for fun; none of the new bytes exist!
        ))))
