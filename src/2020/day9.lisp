(defpackage aoc/2020/day9
  (:use :cl :alexandria :iter :rove :aoc/util))
(in-package :aoc/2020/day9)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day9/")))

(defun parse-numbers (lines)
  (map 'vector #'parse-integer lines))

(defun find-invalid-number (numbers prev-length)
  (iter (for first-i index-of-vector numbers)
    (for next-n = (elt numbers (+ first-i prev-length)))
    (for valid = nil)
    (map-combinations (lambda (comb)
                        (let ((n1 (elt comb 0))
                              (n2 (elt comb 1)))
                          (if (and (/= n1 n2)
                                   (= (+ n1 n2) next-n))
                              (setf valid t))))
                      numbers :start first-i :end (+ first-i prev-length) :length 2 :copy nil)
    (if (not valid) (leave next-n))))

(defun find-invalid-number-file (input-name prev-length)
  (find-invalid-number (parse-numbers (read-data-file-lines input-name)) prev-length))

(deftest find-invalid-number-file
  (ok (= (find-invalid-number-file "test-input-1.txt" 5) 127))
  (ok (= (find-invalid-number-file "input.txt" 25) 14144619)))

(defun find-encryption-weakness (numbers invalid-number)
  (iter (for length from 2 to (length numbers))
    (iter (for first-i to (- (length numbers) length))
      (for sum = (iter (for n in-vector numbers from first-i below (+ first-i length))
                   (sum n)))
      (when (= sum invalid-number)
        (iter (for n in-vector numbers from first-i below (+ first-i length))
          (minimize n into min)
          (maximize n into max)
          (finally (return-from find-encryption-weakness (+ min max))))))))

(defun find-encryption-weakness-file (input-name invalid-number)
  (find-encryption-weakness (parse-numbers (read-data-file-lines input-name)) invalid-number))

(deftest find-encryption-weakness
  (ok (= (find-encryption-weakness-file "test-input-1.txt" 127) 62))
  (ok (= (find-encryption-weakness-file "input.txt" 14144619) 1766397)))
