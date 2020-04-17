(defpackage aoc/2019/day21
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util))
(in-package :aoc/2019/day21)

;;; Part 1

(defparameter *instructions* (list "OR A T"
                                   "AND B T"
                                   "AND C T"
                                   "OR D J"
                                   "NOT T T"
                                   "AND T J"))

(defun execute (ints)
  (let ((input (map 'list #'char-code
                    (apply #'concatenate 'string
                           (mapcan (lambda (s) (list s *newline-str*))
                                   (append *instructions* '("WALK"))))))
        output)
    (intcode:run-computer (intcode:make-computer ints)
                          :input-f (lambda () (pop input))
                          :output-f (lambda (x)
                                      (if (or (>= x char-code-limit)
                                              (not (code-char x)))
                                          (setf output x)
                                          (format t "~a" (code-char x)))))
    output))

;;; Part 2

(defparameter *instructions-2* (list "OR A T"
                                     "AND B T"
                                     "AND C T"
                                     "OR E J"
                                     "OR H J"
                                     "AND D J"
                                     "NOT T T"
                                     "AND T J"))

(defun execute-2 (ints)
  (let ((input (map 'list #'char-code
                    (apply #'concatenate 'string
                           (mapcan (lambda (s) (list s *newline-str*))
                                   (append *instructions-2* '("RUN"))))))
        output)
    (intcode:run-computer (intcode:make-computer ints)
                          :input-f (lambda () (pop input))
                          :output-f (lambda (x)
                                      (if (or (>= x char-code-limit)
                                              (not (code-char x)))
                                          (setf output x)
                                          (format t "~a" (code-char x)))))
    output))
