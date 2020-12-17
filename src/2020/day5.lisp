(defpackage aoc/2020/day5
  (:use :cl :alexandria :iter :rove :aoc/util))
(in-package :aoc/2020/day5)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day5/")))

(defun range-h (l u)
  (let ((a (1+ (- u l))))
    (assert (evenp a))
    (/ a 2)))

(defun seat-location (seat-key)
  (let ((row-l 0)
        (row-u 127)
        (col-l 0)
        (col-u 7))

    (iter (for c in-string (subseq seat-key 0 7))
      (ccase c
        (#\F (setf row-u (- row-u (range-h row-l row-u))))
        (#\B (setf row-l (+ row-l (range-h row-l row-u))))))
    (assert (= row-l row-u))

    (iter (for c in-string (subseq seat-key 7))
      (ccase c
        (#\L (setf col-u (- col-u (range-h col-l col-u))))
        (#\R (setf col-l (+ col-l (range-h col-l col-u))))))
    (assert (= col-l col-u))

    (list row-l col-l)))

(defun seat-id (seat-location)
  (destructuring-bind (row col) seat-location
    (+ (* row 8) col)))

(deftest seat-attributes
  (let ((seat-keys (read-data-file-lines "test-input-1.txt")))
    (ok (equal (seat-location (first seat-keys)) '(44 5)))
    (ok (equal (seat-location (second seat-keys)) '(70 7)))
    (ok (equal (seat-location (third seat-keys)) '(14 7)))
    (ok (equal (seat-location (fourth seat-keys)) '(102 4)))

    (ok (= (seat-id (seat-location (first seat-keys))) 357))
    (ok (= (seat-id (seat-location (second seat-keys))) 567))
    (ok (= (seat-id (seat-location (third seat-keys))) 119))
    (ok (= (seat-id (seat-location (fourth seat-keys))) 820))))

(defun highest-seat-id-file (input-name)
  (iter (for seat-key in (read-data-file-lines input-name))
    (maximize (seat-id (seat-location seat-key)))))

(deftest highest-seat-id-file
  (ok (= (highest-seat-id-file "input.txt") 838)))

(defun seat-id-from-key (seat-key)
  (seat-id (seat-location seat-key)))

(defun all-seat-ids-sorted (seat-keys)
  (sort (iter (for seat-key in seat-keys)
          (collect (seat-id-from-key seat-key)))
        #'<))

(defun missing-seat-id-file (input-name)
  (iter (for id in (all-seat-ids-sorted (read-data-file-lines input-name)))
    (for prev-id previous id)
    (if (and prev-id (/= (1+ prev-id) id))
        (leave (1+ prev-id)))))

(deftest missing-seat-id-file
  (ok (= (missing-seat-id-file "input.txt") 714)))
