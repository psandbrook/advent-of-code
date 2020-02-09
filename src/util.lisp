(defpackage aoc/util
  (:use :cl
        :alexandria
        :genhash)
  (:import-from :str :join)
  (:export :*newline-str*
           :*whitespace*
           :print-debug
           :force-register-test-designator
           :format-hash-table
           :hash-equal
           :hash-combine
           :coord-x
           :coord-y
           :coord))
(in-package :aoc/util)

(defparameter *newline-str* (string #\Newline))
(defparameter *whitespace* (coerce '(#\Space #\Newline #\Tab) 'string))

(defmacro print-debug (form)
  (with-gensyms (form-value)
    `(let ((,form-value ,form))
       (format t "~a: ~s~%" ',form ,form-value) ,form-value)))

(defun force-register-test-designator (test-designator hash-function equal-function)
  (handler-bind ((hash-exists (lambda (c)
                                (declare (ignore c))
                                (invoke-restart ':unregister-and-retry))))
    (register-test-designator test-designator hash-function equal-function)))

(defmethod print-object ((obj hash-table) stream)
  (let ((strings))
    (maphash (lambda (key value) (push (format nil "~s: ~s" key value) strings)) obj)
    (print-unreadable-object (obj stream :type t)
      (format stream "~a" (join ", " strings)))))

(defun hash-equal (ht-1 ht-2 &key (test #'eql))
  (if (/= (hash-table-count ht-1) (hash-table-count ht-2))
      nil
      (progn (maphash (lambda (k-1 v-1)
                        (multiple-value-bind (v-2 present-p) (gethash k-1 ht-2)
                          (if (or (not present-p) (not (funcall test v-1 v-2)))
                              (return-from hash-equal nil))))
                      ht-1)
             t)))

(defun hash-combine (acc value-hash)
  (+ (* 37 acc) value-hash))

(defun coord-x (coord)
  (first coord))

(defun coord-y (coord)
  (second coord))

(defun coord (x y)
  (list x y))
