(defpackage aoc/util
  (:use :cl
        :alexandria
        :genhash)
  (:export :*newline-str*
           :*whitespace*
           :print-debug
           :force-register-test-designator
           :hash-combine
           :coord-x
           :coord-y
           :coord))
(in-package :aoc/util)

(defparameter *newline-str* (string #\Newline))
(defparameter *whitespace* (coerce '(#\Space #\Newline #\Tab) 'string))

(defmacro print-debug (form)
  (let ((form-copy form))
    (once-only (form) `(progn (format t "~a: ~a~%" ',form-copy ,form) ,form))))

(defun force-register-test-designator (test-designator hash-function equal-function)
  (handler-bind ((hash-exists (lambda (c)
                                (declare (ignore c))
                                (invoke-restart ':unregister-and-retry))))
    (register-test-designator test-designator hash-function equal-function)))

(defun hash-combine (acc value-hash)
  (+ (* 37 acc) value-hash))

(defun coord-x (coord)
  (first coord))

(defun coord-y (coord)
  (second coord))

(defun coord (x y)
  (list x y))
