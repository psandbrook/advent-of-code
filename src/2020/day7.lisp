(defpackage aoc/2020/day7
  (:use :cl :alexandria :iter :rove :aoc/util))
(in-package :aoc/2020/day7)

(interpol:enable-interpol-syntax)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day7/")))

(defun parse-contained-part (s)
  (iter (for ss in (str:split ", " s))
    (ppcre:register-groups-bind (num-s name) (#?/^(\d+) (.+) bags?\.?$/ ss :sharedp t)
      (assert (and num-s name))
      (collect (cons name (parse-integer num-s))))))

(defun parse-rules (lines)
  (iter (for line in lines)
    (collect (destructuring-bind (name rest) (str:split " bags contain " line)
               (cons name (parse-contained-part rest))))))

(defun can-contain-bag-n (rules)
  (let ((can-contain nil))
    (iter
      (for saved-length = (length can-contain))
      (iter (for (name . contains) in rules)
        (iter (for contained-name in (cons "shiny gold" can-contain))
          (when (assoc contained-name contains :test #'equal)
            (pushnew name can-contain :test #'equal))))
      (when (= saved-length (length can-contain))
        (leave (length can-contain))))))

(defun can-contain-bag-n-file (input-name)
  (can-contain-bag-n (parse-rules (read-data-file-lines input-name))))

(deftest can-contain-bag-n-file
  (ok (= (can-contain-bag-n-file "test-input-1.txt") 4))
  (ok (= (can-contain-bag-n-file "input.txt") 248)))

(defun count-contained-bags (rules name)
  (iter (for (contained-name . n) in (assoc-value rules name :test #'equal))
    (sum (+ n (* (count-contained-bags rules contained-name) n)))))

(defun count-contained-bags-file (input-name)
  (count-contained-bags (parse-rules (read-data-file-lines input-name)) "shiny gold"))

(deftest count-contained-bags-file
  (ok (= (count-contained-bags-file "test-input-1.txt") 32))
  (ok (= (count-contained-bags-file "test-input-2.txt") 126))
  (ok (= (count-contained-bags-file "input.txt") 57281)))
