(defpackage #:gooptest/examples
  (:use #:cl #:gooptest #:gooptest-avr #:gooptest-arduino-avr)
  (:export #:blink))

(in-package #:gooptest/examples)

(defun sketch-pathname (sketch)
  "Example utility. Returns a pathname to the named arduino sketch's directory.
The argument should be a string designator for the dirname of the sketch
directory, the sketch must be registered in asdf."
  (let ((sketch-name (typecase sketch
                       (symbol (string-downcase sketch))
                       (t (string sketch)))))
    ;; TODO: is there a way to get the directory without converting to
    ;; namestring?
    (directory-namestring
     (asdf:component-pathname
      (asdf:find-component
       '#:gooptest-examples
       (concatenate 'string sketch-name "/" sketch-name ".ino"))))))
