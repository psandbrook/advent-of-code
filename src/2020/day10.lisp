(defpackage aoc/2020/day10
  (:use :cl :alexandria :iter :rove :aoc/util))
(in-package :aoc/2020/day10)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day10/")))

(defun parse-input (lines)
  (let ((out (sort (mapcar #'parse-integer lines) #'<)))
    (nconc out (list (+ (last-elt out) 3)))))

(defun find-answer-1 (joltage-ratings)
  (iter (for r in joltage-ratings)
    (with 1-jolt-diffs = 0)
    (with 3-jolt-diffs = 0)
    (for prev-r previous r initially 0)
    (case (- r prev-r)
      (1 (incf 1-jolt-diffs))
      (3 (incf 3-jolt-diffs)))
    (finally (return (* 1-jolt-diffs 3-jolt-diffs)))))

(defun find-answer-1-file (input-name)
  (find-answer-1 (parse-input (read-data-file-lines input-name))))

(deftest find-answer-1
  (ok (= (find-answer-1-file "test-input-1.txt") 35))
  (ok (= (find-answer-1-file "test-input-2.txt") 220))
  (ok (= (find-answer-1-file "input.txt") 1836)))

(defun count-adapter-chains (joltage-ratings)
  (dexpr joltage-ratings)
  (iter (for r in joltage-ratings)
    (with prev = (list (list 0 1)))
    (setf prev (delete-if (lambda (e) (< (first e) (- r 3))) prev))
    (for min-r-count = (iter (for (prev-r prev-r-count) in prev) (finding prev-r-count minimizing prev-r)))
    (dexpr prev)
    (dexpr r)
    (for r-count = (* min-r-count (ecase (length prev) (1 1) (2 2) (3 4))))
    (dexpr r-count)
    (push (list r r-count) prev)
    (dprint "")
    (finally (return (iter (for (r r-count) in prev) (finding r-count maximizing r))))))
