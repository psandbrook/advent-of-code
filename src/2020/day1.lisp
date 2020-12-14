(defpackage aoc/2020/day1
  (:use :cl :rove :iter :aoc/util))
(in-package :aoc/2020/day1)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day1/")))

(defun find-answer-entries (path)
  (let ((entries (iter (for l in (read-data-file-lines path))
                   (collect (parse-integer l)))))
    (iter
      (for n1 in entries)
      (for i upfrom 0)
      (iter
        (for n2 in entries)
        (for j upfrom 0)
        (if (and (not (= i j)) (= (+ n1 n2) 2020))
            (return-from find-answer-entries (* n1 n2)))))))

(defun find-answer-entries-2 (path)
  (let ((entries (iter (for l in (read-data-file-lines path))
                   (collect (parse-integer l)))))
    (iter
      (for n1 in entries)
      (for i upfrom 0)
      (iter
        (for n2 in entries)
        (for j upfrom 0)
        (iter
          (for n3 in entries)
          (for k upfrom 0)
          (if (and (not (= i j k)) (= (+ n1 n2 n3) 2020))
              (return-from find-answer-entries-2 (* n1 n2 n3))))))))

(deftest day1-test
  (testing "find-answer-entries"
    (ok (= (find-answer-entries "test-input-1.txt") 514579)))

  (testing "find-answer-entries-2"
    (ok (= (find-answer-entries-2 "test-input-1.txt") 241861950))))
