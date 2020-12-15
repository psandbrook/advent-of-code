(defpackage aoc/2020/day2
  (:use :cl :rove :iter :aoc/util))
(in-package :aoc/2020/day2)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day2/")))

(defun parse-line (line)
  (let ((strings (delete "" (ppcre:split "[- :]" line) :test #'equal)))
    (destructuring-bind (low high char str) strings
      (list (parse-integer low) (parse-integer high) (character char) str))))

(defun valid-passwords-n (path)
  (let ((lines (read-data-file-lines path))
        (count-f #'count))
    (iter (for line in lines)
      (destructuring-bind (low high char str) (parse-line line)
        (counting (<= low (funcall count-f char str) high))))))

(defun valid-passwords-n-2 (path)
  (let ((lines (read-data-file-lines path)))
    (iter (for line in lines)
      (destructuring-bind (low high char str) (parse-line line)
        (let ((low-t (eql (elt str (1- low)) char))
              (high-t (eql (elt str (1- high)) char)))
          (counting (and (or low-t high-t) (not (and low-t high-t)))))))))

(deftest day2-test
  (testing "valid-passwords-n"
    (ok (= (valid-passwords-n "test-input-1.txt") 2)))

  (testing "valid-passwords-n-2"
    (ok (= (valid-passwords-n-2 "test-input-1.txt") 1))))
