(defpackage aoc/2019/day10
  (:use :cl
        :3d-vectors
        :uiop
        :rove
        :aoc/util))
(in-package :aoc/2019/day10)

;;; Part 1

(defun obstructs (coord-a coord-b coord-r)
  (destructuring-bind ((x-a y-a) (x-b y-b) (x-r y-r)) (list coord-a coord-b coord-r)
    (let* ((l-x-denom (- x-b x-a))
           (l-y-denom (- y-b y-a))
           (l-x (if (/= l-x-denom 0) (/ (- x-r x-a) l-x-denom)))
           (l-y (if (/= l-y-denom 0) (/ (- y-r y-a) l-y-denom))))
      (cond ((= l-x-denom 0) (and (= x-r x-a) (<= 0 l-y 1)))
            ((= l-y-denom 0) (and (= y-r y-a) (<= 0 l-x 1)))
            ((= l-x l-y) (<= 0 l-x 1))
            (t nil)))))

(defun parse-asteroid-map-str (str)
  (setf str (string-right-trim *newline-str* str))
  (loop
    with coords = ()
    with lines = (split-string str :separator *newline-str*)
    for line in lines
    for y below (length lines)
    do (loop
         for c across line
         for x below (length line)
         do (ecase c
              (#\.)
              (#\# (push (list x y) coords))))
    finally (return coords)))

(defun count-detected-asteroids (asteroids)
  (loop
    with result = (make-hash-table :test #'equalp)
    for coord in asteroids
    for (x y) = coord
    do (setf (gethash coord result) 0)
    (loop
      for coord-2 in asteroids
      for (x-2 y-2) = coord-2
      if (not (equalp coord coord-2))
      do (loop
           for coord-3 in asteroids
           for (x-3 y-3) = coord-3
           if (and (not (equalp coord coord-3))
                   (not (equalp coord-2 coord-3))
                   (obstructs coord coord-2 coord-3))
           do (return)
           finally (incf (gethash coord result))))
    finally (return result)))

(defun best-station (asteroids)
  (loop
    with max-asteroids = -1
    with best-station
    for coord being the hash-keys in (count-detected-asteroids asteroids) using (hash-value count)
    if (> count max-asteroids) do (setf max-asteroids count) (setf best-station coord)
    finally (return (list best-station max-asteroids))))

(deftest part-1-test
  (testing "best-station"
    (ok (equalp (best-station (parse-asteroid-map-str (concatenate 'string
                                                                   ".#..#" *newline-str*
                                                                   "....." *newline-str*
                                                                   "#####" *newline-str*
                                                                   "....#" *newline-str*
                                                                   "...##")))
                '((3 4) 8)))
    (ok (equalp (best-station (parse-asteroid-map-str (concatenate 'string
                                                                   "......#.#." *newline-str*
                                                                   "#..#.#...." *newline-str*
                                                                   "..#######." *newline-str*
                                                                   ".#.#.###.." *newline-str*
                                                                   ".#..#....." *newline-str*
                                                                   "..#....#.#" *newline-str*
                                                                   "#..#....#." *newline-str*
                                                                   ".##.#..###" *newline-str*
                                                                   "##...#..#." *newline-str*
                                                                   ".#....####")))
                '((5 8) 33)))
    (ok (equalp (best-station (parse-asteroid-map-str (concatenate 'string
                                                                   "#.#...#.#." *newline-str*
                                                                   ".###....#." *newline-str*
                                                                   ".#....#..." *newline-str*
                                                                   "##.#.#.#.#" *newline-str*
                                                                   "....#.#.#." *newline-str*
                                                                   ".##..###.#" *newline-str*
                                                                   "..#...##.." *newline-str*
                                                                   "..##....##" *newline-str*
                                                                   "......#..." *newline-str*
                                                                   ".####.###.")))
                '((1 2) 35)))
    (ok (equalp (best-station (parse-asteroid-map-str (concatenate 'string
                                                                   ".#..#..###" *newline-str*
                                                                   "####.###.#" *newline-str*
                                                                   "....###.#." *newline-str*
                                                                   "..###.##.#" *newline-str*
                                                                   "##.##.#.#." *newline-str*
                                                                   "....###..#" *newline-str*
                                                                   "..#.#..#.#" *newline-str*
                                                                   "#..#.#.###" *newline-str*
                                                                   ".##...##.#" *newline-str*
                                                                   ".....#.#..")))
                '((6 3) 41)))
    (ok (equalp (best-station (parse-asteroid-map-str (concatenate 'string
                                                                   ".#..##.###...#######" *newline-str*
                                                                   "##.############..##." *newline-str*
                                                                   ".#.######.########.#" *newline-str*
                                                                   ".###.#######.####.#." *newline-str*
                                                                   "#####.##.#.##.###.##" *newline-str*
                                                                   "..#####..#.#########" *newline-str*
                                                                   "####################" *newline-str*
                                                                   "#.####....###.#.#.##" *newline-str*
                                                                   "##.#################" *newline-str*
                                                                   "#####.##.###..####.." *newline-str*
                                                                   "..######..##.#######" *newline-str*
                                                                   "####.##.####...##..#" *newline-str*
                                                                   ".#####..#.######.###" *newline-str*
                                                                   "##...#.##########..." *newline-str*
                                                                   "#.##########.#######" *newline-str*
                                                                   ".####.#.###.###.#.##" *newline-str*
                                                                   "....##.##.###..#####" *newline-str*
                                                                   ".#.#.###########.###" *newline-str*
                                                                   "#.#.#.#####.####.###" *newline-str*
                                                                   "###.##.####.##.#..##")))
                '((11 13) 210)))))

;;; Part 2

(defparameter *default-epsilon* 0.00000000000001d0)

(defun float-eq (a b &optional (epsilon *default-epsilon*))
  (if (and (< a 1) (< b 1))
      (<= (abs (- a b)) epsilon)
      (<= (abs (- a b)) (* (max (abs a) (abs b)) epsilon))))

(defun coord-to-vec (coord)
  (apply #'vec2 coord))

(defun clockwise-angle (v1 v2)
  (with-vec2 (x1 y1) v1
    (with-vec2 (x2 y2) v2
      (+ (atan (- (- (* x1 y2) (* y1 x2))) (- (v. v1 v2)))
         pi))))

(defun calc-200th-asteroid-vaporized (asteroids station)
  (setf asteroids (copy-list asteroids))
  (loop
    with vaporized-count = 0
    with laser-v = (vec2 0 -1)
    with include-0-angle = t
    for coord-to-vaporize = nil
    do (loop
         with epsilon = 0.0000001d0
         with min-angle
         for coord in asteroids
         for v = (v- (coord-to-vec coord) (coord-to-vec station))
         for angle = (clockwise-angle laser-v v)
         if (and (or (not min-angle)
                     (< angle min-angle)
                     (and (float-eq angle min-angle epsilon)
                          (< (vlength v) (vlength (v- (coord-to-vec coord-to-vaporize) (coord-to-vec station))))))
                 (or include-0-angle (not (float-eq angle 0 epsilon))))
         do (setf min-angle angle)
         (setf coord-to-vaporize coord))

    if coord-to-vaporize
    do (setf asteroids (delete coord-to-vaporize asteroids :test #'equalp))
    (setf laser-v (v- (coord-to-vec coord-to-vaporize) (coord-to-vec station)))
    (incf vaporized-count)
    (if (= vaporized-count 200) (return coord-to-vaporize))

    if include-0-angle do (setf include-0-angle nil)

    if (emptyp asteroids) do (error "no asteroids left")
    else if (not coord-to-vaporize) do (setf include-0-angle t)))

(defun find-part-2-answer (asteroids)
  (destructuring-bind (station num-asteroids) (best-station asteroids)
    (declare (ignore num-asteroids))
    (let ((asteroids-rest (remove station asteroids :test #'equalp)))
      (destructuring-bind (x y) (calc-200th-asteroid-vaporized asteroids-rest station)
        (+ (* x 100) y)))))

(deftest part-2-test
  (testing "calc-200th-asteroid-vaporized"
    (ok (eql (find-part-2-answer (parse-asteroid-map-str (concatenate 'string
                                                                      ".#..##.###...#######" *newline-str*
                                                                      "##.############..##." *newline-str*
                                                                      ".#.######.########.#" *newline-str*
                                                                      ".###.#######.####.#." *newline-str*
                                                                      "#####.##.#.##.###.##" *newline-str*
                                                                      "..#####..#.#########" *newline-str*
                                                                      "####################" *newline-str*
                                                                      "#.####....###.#.#.##" *newline-str*
                                                                      "##.#################" *newline-str*
                                                                      "#####.##.###..####.." *newline-str*
                                                                      "..######..##.#######" *newline-str*
                                                                      "####.##.####...##..#" *newline-str*
                                                                      ".#####..#.######.###" *newline-str*
                                                                      "##...#.##########..." *newline-str*
                                                                      "#.##########.#######" *newline-str*
                                                                      ".####.#.###.###.#.##" *newline-str*
                                                                      "....##.##.###..#####" *newline-str*
                                                                      ".#.#.###########.###" *newline-str*
                                                                      "#.#.#.#####.####.###" *newline-str*
                                                                      "###.##.####.##.#..##")))
             802))))
