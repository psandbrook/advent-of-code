(defpackage aoc/util
  (:use :cl
        :alexandria
        :genhash)
  (:import-from :str :concat :join)
  (:export :*newline-str*
           :*whitespace*
           :dexpr
           :dprint
           :force-register-test-designator
           :format-hash-table
           :hash-equal
           :hash-combine
           :rpop
           :make-extendable-vector
           :extendable-vector
           :extendable-string
           :coord-x
           :coord-y
           :coord
           :make-queue
           :queue-empty
           :queue-push
           :queue-pop
           :queue-front))
(in-package :aoc/util)

(defparameter *newline-str* (string #\Newline))
(defparameter *whitespace* (coerce '(#\Space #\Newline #\Tab) 'string))

(defmacro dexpr (form)
  (with-gensyms (form-value)
    `(let ((,form-value ,form))
       (format *error-output* "~s: ~s~%" ',form ,form-value)
       ,form-value)))

(defun dprint (str)
  (format *error-output* (concat str "~%")))

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

(defmacro rpop (place)
  (with-gensyms (last-2)
    `(let ((,last-2 (last ,place 2)))
       (if (eq (cdr ,last-2) nil)
           (prog1 (car ,last-2)
             (setf ,place nil))
           (prog1 (cadr ,last-2)
             (setf (cdr ,last-2) nil))))))

(defun make-extendable-vector (objects &key (element-type t))
  (make-array (length objects) :element-type element-type :initial-contents objects :adjustable t :fill-pointer t))

(defun extendable-vector (&rest objects)
  (make-extendable-vector objects))

(defun extendable-string (&rest chars)
  (make-extendable-vector chars :element-type 'character))

(defun coord-x (coord)
  (first coord))

(defun coord-y (coord)
  (second coord))

(defun coord (x y)
  (list x y))

(defun make-queue ()
  (cons nil nil))

(defun queue-empty (q)
  (not (car q)))

(defun queue-push (q e)
  (if (queue-empty q)
      (progn
        (setf (car q) (cons e nil))
        (setf (cdr q) (car q)))
      (progn
        (setf (cddr q) (cons e nil))
        (setf (cdr q) (cddr q)))))

(defun queue-pop (q)
  (let ((e (caar q)))
    (setf (car q) (cadr q))
    (if (not (car q))
        (setf (cdr q) nil))
    e))

(defun queue-front (q)
  (car q))
