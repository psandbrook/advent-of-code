(defpackage aoc/2020/day6
  (:use :cl :alexandria :iter :rove :aoc/util))
(in-package :aoc/2020/day6)

(defun read-data-file-string (name)
  (uiop:read-file-string (merge-pathnames name "data/2020/day6/")))

(defun questions-yes-n (group-answers)
  (length (iter (for c in-string group-answers)
            (if (char<= #\a c #\z)
                (adjoining c)))))

(defun sum-all-questions-yes-file (input-name)
  (let ((all-group-answers (str:split (coerce '(#\Newline #\Newline) 'string) (read-data-file-string input-name))))
    (iter (for group-answers in all-group-answers)
      (sum (questions-yes-n group-answers)))))

(deftest sum-all-questions-yes-file
  (ok (= (sum-all-questions-yes-file "test-input-1.txt") 11))
  (ok (= (sum-all-questions-yes-file "input.txt") 6585)))

(defun questions-everyone-yes-n (group-answers)
  (let* ((all-person-answers (str:lines group-answers))
         (yes-acc (coerce (first all-person-answers) 'list)))
    (iter (for person-answers in (rest all-person-answers))
      (setf yes-acc (nintersection yes-acc (coerce person-answers 'list))))
    (length yes-acc)))

(defun sum-all-questions-everyone-yes-file (input-name)
  (let ((all-group-answers (str:split (coerce '(#\Newline #\Newline) 'string) (read-data-file-string input-name))))
    (iter (for group-answers in all-group-answers)
      (sum (questions-everyone-yes-n group-answers)))))

(deftest sum-all-questions-everyone-yes-file
  (ok (= (sum-all-questions-everyone-yes-file "test-input-1.txt") 6))
  (ok (= (sum-all-questions-everyone-yes-file "input.txt") 3276)))
