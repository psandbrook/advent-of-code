(defpackage aoc/util
  (:use :cl
        :alexandria)
  (:export :*newline-str*
           :print-debug
           :coord-x
           :coord-y
           :coord))
(in-package :aoc/util)

;;; Part 1

(defparameter *newline-str* (string #\Newline))

(defmacro print-debug (form)
  (let ((form-copy form))
    (once-only (form) `(progn (format t "~a: ~a~%" ',form-copy ,form) ,form))))

(defun coord-x (coord)
  (first coord))

(defun coord-y (coord)
  (second coord))

(defun coord (x y)
  (list x y))
