(defpackage aoc/2019/day1
  (:use :cl
        :rove))
(in-package :aoc/2019/day1)

;;; Part 1

(defun required-fuel (mass)
  (- (floor (/ mass 3.0)) 2))

(defun total-required-fuel (masses)
  (apply #'+ (mapcar #'required-fuel masses)))

(deftest part-1-test
  (testing "required-fuel"
           (ok (= (required-fuel 12) 2))
           (ok (= (required-fuel 14) 2))
           (ok (= (required-fuel 1969) 654))
           (ok (= (required-fuel 100756) 33583))))

;;; Part 2

(defun complete-required-fuel (mass)
  (loop
    with total = 0
    with cur = mass
    for fuel = (required-fuel cur)
    if (<= fuel 0) return total
    else do (incf total fuel) (setf cur fuel)))

(defun total-required-fuel-2 (masses)
  (apply #'+ (mapcar #'complete-required-fuel masses)))

(deftest part-2-test
  (testing "complete-required-fuel"
           (ok (= (complete-required-fuel 14) 2))
           (ok (= (complete-required-fuel 1969) 966))
           (ok (= (complete-required-fuel 100756) 50346))))
