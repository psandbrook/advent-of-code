(defpackage aoc/2020/day3
  (:use :cl :rove :iter :aoc/util))
(in-package :aoc/2020/day3)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day3/")))

(defun parse-grid (lines)
  (let ((grid (make-array (list (length lines) (length (first lines))) :initial-element nil)))
    (iter
      (for line in lines)
      (for y upfrom 0)
      (iter
        (for c in-string line)
        (for x upfrom 0)
        (if (eql c #\#) (setf (aref grid y x) t))))
    grid))

(defun print-grid (grid)
  (iter (for y below (array-dimension grid 0))
    (iter (for x below (array-dimension grid 1))
      (format t "~a" (if (aref grid y x) #\# #\.)))
    (format t "~%")))

(defun grid-in-bounds-p (grid y)
  (< y (array-dimension grid 0)))

(defun grid-at (grid y x)
  (setf x (mod x (array-dimension grid 1)))
  (aref grid y x))

(defun count-trees-of-slope (grid)
  (iter
    (for y upfrom 0)
    (for x upfrom 0 by 3)
    (while (grid-in-bounds-p grid y))
    (counting (grid-at grid y x))))

(defun count-trees-of-slope-file (name)
  (count-trees-of-slope (parse-grid (read-data-file-lines name))))

(defun count-trees-of-slope-2 (grid y-step x-step)
  (iter
    (for y upfrom 0 by y-step)
    (for x upfrom 0 by x-step)
    (while (grid-in-bounds-p grid y))
    (counting (grid-at grid y x))))

(defun find-answer-2 (input-name)
  (let ((grid (parse-grid (read-data-file-lines input-name))))
    (iter (for (x-step y-step) in '((1 1) (3 1) (5 1) (7 1) (1 2)))
      (multiply (count-trees-of-slope-2 grid y-step x-step)))))

(deftest day3-test
  (testing "count-trees-of-slope-file"
    (ok (= (count-trees-of-slope-file "test-input-1.txt") 7)))
  (testing "find-answer-2"
    (ok (= (find-answer-2 "test-input-1.txt") 336))))
