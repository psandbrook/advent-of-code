(defpackage aoc/2018/day1
  (:use :cl
        :rove))
(in-package :aoc/2018/day1)

;;; Part 1

(defun calc-freq (freq-changes)
  (apply #'+ freq-changes))

(deftest calc-freq-test
  (testing "calc-freq"
    (ok (= (calc-freq '(1 -2 3 1)) 3))
    (ok (= (calc-freq '(1 1 1)) 3))
    (ok (= (calc-freq '(1 1 -2)) 0))
    (ok (= (calc-freq '(-1 -2 -3)) -6))))

;;; Part 2

(defun first-dup-frequency (freq-changes)
  (loop
    for cur-cons = freq-changes then (cdr cur-cons)
    if (not cur-cons) do (setf cur-cons freq-changes)
    with ch
    do (setf ch (car cur-cons))
    sum ch into fr
    with frs = '(0)
    if (member fr frs) return fr
    else do (push fr frs)))

(deftest first-dup-frequency-test
  (testing "first-dup-frequency"
    (ok (= (first-dup-frequency '(1 -2 3 1)) 2))
    (ok (= (first-dup-frequency '(1 -1)) 0))
    (ok (= (first-dup-frequency '(3 3 4 -2 -4)) 10))
    (ok (= (first-dup-frequency '(-6 3 8 5 -6)) 5))
    (ok (= (first-dup-frequency '(7 7 -2 -7 -4)) 14))))
