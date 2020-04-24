(defpackage aoc/2019/day25
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util))
(in-package :aoc/2019/day25)

;;; Part 1

(defun execute (ints)
  (let (current-input)
    (intcode:run-computer
      (intcode:make-computer ints)
      :input-f (lambda ()
                 (if (not current-input)
                     (setf current-input (map 'list #'char-code (str:concat (read-line) *newline-str*))))
                 (let ((x (pop current-input)))
                   (format t "~a" (code-char x))
                   x))
      :output-f (lambda (x) (format t "~a" (code-char x))))))
