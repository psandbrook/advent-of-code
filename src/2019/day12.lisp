(defpackage aoc/2019/day12
  (:use :cl
        :iterate
        :uiop
        :rove
        :aoc/util))
(in-package :aoc/2019/day12)

;;; Part 1

(defclass ivec3 ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (z :initarg :z :accessor z)))

(defun make-ivec3 (x y z)
  (make-instance 'ivec3 :x x :y y :z z))

(defmethod print-object ((obj ivec3) stream)
  (print-unreadable-object (obj stream :type t) (format stream "x: ~a, y: ~a, z: ~a" (x obj) (y obj) (z obj))))

(defun copy-ivec3 (vec)
  (make-ivec3 (x vec) (y vec) (z vec)))

(defun ivec3+ (v1 v2)
  (make-ivec3 (+ (x v1) (x v2)) (+ (y v1) (y v2)) (+ (z v1) (z v2))))

(defun ivec3= (&rest vecs)
  (iter (for sub-vecs on vecs)
        (while (cdr sub-vecs))
        (for v1 = (car sub-vecs))
        (for v2 = (cadr sub-vecs))
        (always (and (= (x v1) (x v2))
                     (= (y v1) (y v2))
                     (= (z v1) (z v2))))))

(defun moons= (&rest moons-list)
  (iter (for sub-moons-list on moons-list)
        (while (cdr sub-moons-list))
        (for moons1 = (car sub-moons-list))
        (for moons2 = (cadr sub-moons-list))
        (always (iter (for (pos1 v1) in moons1)
                      (for (pos2 v2) in moons2)
                      (always (and (ivec3= pos1 pos2)
                                   (ivec3= v1 v2)))))))

(defun parse-inner-integer (s)
  (parse-integer (delete-if (lambda (c) (not (or (digit-char-p c) (find c (strcat "-" *whitespace*)))))
                            s)))

(defun parse-positions (str)
  (let ((lines (split-string (string-right-trim *newline-str* str) :separator *newline-str*)))
    (iter (for line in lines)
          (for (x y z) = (iter (for e in (split-string line))
                               (collect (parse-inner-integer e))))
          (collect (list (make-ivec3 x y z) (make-ivec3 0 0 0))))))

(defun step-sim (moons)
  (flet ((new-velocity (a b)
           (cond ((< a b) (list 1 -1))
                 ((> a b) (list -1 1))
                 (t (list 0 0)))))
    (iter (for sub-moons on moons)
          (while (cdr sub-moons))
          (for (pos1 v1) = (car sub-moons))
          (iter (for (pos2 v2) in (cdr sub-moons))
            (destructuring-bind (x1 x2) (new-velocity (x pos1) (x pos2))
              (incf (x v1) x1)
              (incf (x v2) x2))
            (destructuring-bind (y1 y2) (new-velocity (y pos1) (y pos2))
              (incf (y v1) y1)
              (incf (y v2) y2))
            (destructuring-bind (z1 z2) (new-velocity (z pos1) (z pos2))
              (incf (z v1) z1)
              (incf (z v2) z2)))))
  (iter (for moon in moons)
        (for (pos v) = moon)
        (setf (car moon) (ivec3+ pos v))))

(defun step-sim-n (moons n)
  (iter (repeat n) (step-sim moons)))

(defun total-energy (moons)
  (iter (for (pos v) in moons)
        (for pot = (+ (abs (x pos)) (abs (y pos)) (abs (z pos))))
        (for kin = (+ (abs (x v)) (abs (y v)) (abs (z v))))
        (sum (* pot kin))))

(deftest part-1-test
  (testing "step-sim-n"
    (let ((moons (parse-positions (strcat "<x=-1, y=0, z=2>" *newline-str*
                                          "<x=2, y=-10, z=-7>" *newline-str*
                                          "<x=4, y=-8, z=8>" *newline-str*
                                          "<x=3, y=5, z=-1>")))
          (moons-expected (list (list (make-ivec3 2 1 -3) (make-ivec3 -3 -2 1))
                                (list (make-ivec3 1 -8 0) (make-ivec3 -1 1 3))
                                (list (make-ivec3 3 -6 1) (make-ivec3 3 2 -3))
                                (list (make-ivec3 2 0 4) (make-ivec3 1 -1 -1)))))
      (step-sim-n moons 10)
      (ok (moons= moons moons-expected))
      (ok (= (total-energy moons) 179)))))

;;; Part 2

(defun axis-moons= (&rest axis-moons-list)
  (iter (for sub-moons-list on axis-moons-list)
        (while (cdr sub-moons-list))
        (for moons1 = (car sub-moons-list))
        (for moons2 = (cadr sub-moons-list))
        (always (iter (for (pos1 v1) in moons1)
                      (for (pos2 v2) in moons2)
                      (always (and (= pos1 pos2)
                                   (= v1 v2)))))))

(defun step-sim-axis (moons)
  (iter (for sub-moons on moons)
        (while (cdr sub-moons))
        (for moon1 = (car sub-moons))
        (for (pos1 v1) = moon1)
        (iter (for moon2 in (cdr sub-moons))
              (for (pos2 v2) = moon2)
              (cond ((< pos1 pos2) (incf (second moon1))
                                   (decf (second moon2)))
                    ((> pos1 pos2) (decf (second moon1))
                                   (incf (second moon2))))))
  (iter (for moon in moons)
        (for (pos v) = moon)
        (setf (car moon) (+ pos v))))

(defun find-axis-period (axis-moons)
  (flet ((copy-axis-moons (axis-moons)
           (iter (for moon in axis-moons)
                 (collect (copy-list moon)))))
    (let ((init-moons (copy-axis-moons axis-moons))
          (moons (copy-axis-moons axis-moons)))
      (iter (for n from 0)
        (if-first-time nil (if (axis-moons= moons init-moons) (leave n)))
        (step-sim-axis moons)))))

(defun find-duplicate-state (moons)
  (flet ((to-axis-moons (moons accessor)
           (iter (for (pos v) in moons)
                 (collect (list (funcall accessor pos) (funcall accessor v))))))
    (let ((x-period (find-axis-period (to-axis-moons moons #'x)))
          (y-period (find-axis-period (to-axis-moons moons #'y)))
          (z-period (find-axis-period (to-axis-moons moons #'z))))
      (lcm x-period y-period z-period))))

(deftest part-2-test
  (testing "find-duplicate-state"
    (ok (= (find-duplicate-state (parse-positions (strcat "<x=-1, y=0, z=2>" *newline-str*
                                                          "<x=2, y=-10, z=-7>" *newline-str*
                                                          "<x=4, y=-8, z=8>" *newline-str*
                                                          "<x=3, y=5, z=-1>")))
           2772))
    (ok (= (find-duplicate-state (parse-positions (strcat "<x=-8, y=-10, z=0>" *newline-str*
                                                          "<x=5, y=5, z=10>" *newline-str*
                                                          "<x=2, y=-7, z=3>" *newline-str*
                                                          "<x=9, y=-8, z=-3>")))
           4686774924))))
