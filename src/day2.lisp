(defpackage aoc/day2
  (:use :cl
        :rove))
(in-package :aoc/day2)

;;; Part 1

(defun count-id (id)
  (let ((char-counts (make-hash-table)))
    (loop
      for c across id
      if (not (gethash c char-counts)) do (setf (gethash c char-counts) 0)
      do (incf (gethash c char-counts)))
    (loop
      with two = nil
      with three = nil
      for ct being the hash-values in char-counts
      if (= ct 2) do (setf two t)
      else if (= ct 3) do (setf three t)
      finally (return (list two three)))))

(defun checksum (ids)
  (loop
    for id in ids
    for (two three) = (count-id id)
    count two into two-count
    count three into three-count
    finally (return (* two-count three-count))))

(deftest part-1-test
  (testing "count-id"
    (ok (equalp (count-id "abcdef") '(nil nil)))
    (ok (equalp (count-id "bababc") '(t t)))
    (ok (equalp (count-id "abbcde") '(t nil)))
    (ok (equalp (count-id "abcccd") '(nil t)))
    (ok (equalp (count-id "aabcdd") '(t nil)))
    (ok (equalp (count-id "abcdee") '(t nil)))
    (ok (equalp (count-id "ababab") '(nil t))))
  (testing "checksum"
    (ok (= (checksum '("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab")) 12))))

;;; Part 2

(defun compare-ids (id1 id2)
  (loop
    with diff = nil
    for c1 across id1
    for c2 across id2
    for i = 0 then (1+ i)
    if (not (eql c1 c2))
      if diff return nil else do (setf diff i)
    finally (return diff)))

(defun ids-common-chars (ids)
  (loop named outer
    for id1 in ids
    do (loop
         for id2 in (remove id1 ids :test #'equal)
         for i = (compare-ids id1 id2)
         if i do (return-from outer (remove-if (constantly t) id1 :start i :count 1)))))

(deftest part-2-test
  (testing "compare-ids"
    (ng (compare-ids "abcde" "abcde"))
    (ng (compare-ids "abcde" "axcye"))
    (ok (compare-ids "fghij" "fguij")))
  (testing "ids-common-chars"
    (ok (equal (ids-common-chars '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz")) "fgij"))))
