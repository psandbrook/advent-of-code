(defpackage aoc/2019/day3
  (:use :cl
        :uiop
        :genhash
        :rove))
(in-package :aoc/2019/day3)

;;; Part 1

(defun hash-combine (acc value-hash)
  (+ (* 37 acc) value-hash))

(defclass coord ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defun make-coord (x y)
  (make-instance 'coord :x x :y y))

(defmethod print-object ((obj coord) stream)
  (print-unreadable-object (obj stream :type t) (format stream "x: ~a, y: ~a" (x obj) (y obj))))

(defun copy-coord (coord)
  (make-coord (x coord) (y coord)))

(defun coord-equal (&rest coords)
  (and (apply #'= (mapcar #'x coords)) (apply #'= (mapcar #'y coords))))

(defun coord-hash (coord)
  (let ((result 1))
    (setf result (hash-combine result (x coord)))
    (setf result (hash-combine result (y coord)))
    result))

(defvar *registered-coord-hash-test* nil)
(when (not *registered-coord-hash-test*)
  (register-test-designator 'coord #'coord-hash #'coord-equal)
  (setf *registered-coord-hash-test* t))

(defparameter *origin* (make-coord 0 0))

(defun parse-path-str (str)
  (flet ((parse-dir-str (str)
           (let ((dir (ecase (elt str 0)
                        (#\R 'right)
                        (#\U 'up)
                        (#\L 'left)
                        (#\D 'down)))
                 (amount (parse-integer (subseq str 1))))
             (list (list dir amount)))))
    (mapcan #'parse-dir-str (split-string str :separator ","))))

(defun wire-path (instructions)
  (loop
    with cur-coord = (make-coord 0 0)
    with path = (list (copy-coord cur-coord))
    for (dir amount) in instructions
    do (ecase dir
         (right (incf (x cur-coord) amount))
         (up (incf (y cur-coord) amount))
         (left (decf (x cur-coord) amount))
         (down (decf (y cur-coord) amount)))
    (push (copy-coord cur-coord) path)
    finally (return path)))

(defun closest-intersection-distance (instrs1 instrs2)
  (let ((path1 (wire-path instrs1))
        (path2 (wire-path instrs2))
        (intersects ()))
    (loop
      for cur-cons-1 = path1 then (cdr cur-cons-1)
      if (not (cdr cur-cons-1)) do (return)
      do (loop
           with x1
           with x2
           with x3
           with x4
           with y1
           with y2
           with y3
           with y4
           with denom

           for cur-cons-2 = path2 then (cdr cur-cons-2)
           if (not (cdr cur-cons-2)) do (return)

           do (setf x1 (x (car cur-cons-1)))
           (setf x2 (x (cadr cur-cons-1)))
           (setf x3 (x (car cur-cons-2)))
           (setf x4 (x (cadr cur-cons-2)))
           (setf y1 (y (car cur-cons-1)))
           (setf y2 (y (cadr cur-cons-1)))
           (setf y3 (y (car cur-cons-2)))
           (setf y4 (y (cadr cur-cons-2)))
           (setf denom (- (* (- x1 x2) (- y3 y4))
                          (* (- y1 y2) (- x3 x4))))

           if (/= denom 0) do (let ((param-t (/ (- (* (- x1 x3) (- y3 y4))
                                                   (* (- y1 y3) (- x3 x4)))
                                                denom))
                                    (param-u (- (/ (- (* (- x1 x2) (- y1 y3))
                                                      (* (- y1 y2) (- x1 x3)))
                                                   denom))))
                                (if (and (<= 0 param-t 1) (<= 0 param-u 1))
                                    (push (make-coord (+ x1 (* param-t (- x2 x1)))
                                                      (+ y1 (* param-t (- y2 y1))))
                                          intersects)))))

    (setf intersects (delete-if #'(lambda (coord) (coord-equal *origin* coord)) intersects))
    (apply #'min (mapcar #'(lambda (coord) (+ (abs (x coord)) (abs (y coord)))) intersects))))

(deftest part-1-test
  (testing "closest-intersection-distance"
           (ok (= (closest-intersection-distance (parse-path-str "R8,U5,L5,D3") (parse-path-str "U7,R6,D4,L4")) 6))
           (ok (= (closest-intersection-distance (parse-path-str "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                                                 (parse-path-str "U62,R66,U55,R34,D71,R55,D58,R83"))
                  159))
           (ok (= (closest-intersection-distance (parse-path-str "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
                                                 (parse-path-str "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
                  135))))

;;; Part 2

(defun calc-steps (instructions)
  (loop
    with cur-coord = (make-coord 0 0)
    with steps-n = 0
    with steps = (make-generic-hashtable :test 'coord)
    for (dir amount) in instructions
    do (loop
         repeat amount
         do (ecase dir
              (right (incf (x cur-coord)))
              (up (incf (y cur-coord)))
              (left (decf (x cur-coord)))
              (down (decf (y cur-coord))))
         (incf steps-n)
         (if (not (hashref cur-coord steps)) (setf (hashref (copy-coord cur-coord) steps) steps-n)))
    finally (return steps)))

(defun min-intersection-steps (instrs1 instrs2)
  (let ((steps1 (calc-steps instrs1))
        (steps2 (calc-steps instrs2))
        (intersection-steps ()))
    (hashmap #'(lambda (coord2 steps-n-2)
                 (let ((result (hashref coord2 steps1)))
                   (if result (push (+ result steps-n-2) intersection-steps))))
             steps2)
    (apply #'min intersection-steps)))

(deftest part-2-test
  (testing "min-intersection-steps"
           (ok (= (min-intersection-steps (parse-path-str "R8,U5,L5,D3") (parse-path-str "U7,R6,D4,L4"))
                  30))
           (ok (= (min-intersection-steps (parse-path-str "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                                          (parse-path-str "U62,R66,U55,R34,D71,R55,D58,R83"))
                  610))
           (ok (= (min-intersection-steps (parse-path-str "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
                                          (parse-path-str "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
                  410))))
