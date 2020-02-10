(defpackage aoc/2019/day15
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util
        :aoc/2019/intcode)
  (:import-from :str :join))
(in-package :aoc/2019/day15)

;;; Parts 1 & 2

(defun expand-grid (grid)
  (let* ((new-dimensions (mapcar (lambda (d) (+ d 2)) (array-dimensions grid)))
         (new-grid (make-array new-dimensions :element-type 'symbol :initial-element 'unexplored)))
    (iter (for x below (array-dimension grid 0))
          (iter (for y below (array-dimension grid 1))
                (setf (aref new-grid (1+ x) (1+ y)) (aref grid x y))))
    new-grid))

(defun coord-to-indices (grid pos)
  (list (+ (floor (array-dimension grid 0) 2) (coord-x pos))
        (+ (floor (array-dimension grid 1) 2) (coord-y pos))))

(defun grid-aref (grid pos)
  (destructuring-bind (x y) (coord-to-indices grid pos)
    (aref grid x y)))

(defun (setf grid-aref) (value grid pos)
  (destructuring-bind (x y) (coord-to-indices grid pos)
    (setf (aref grid x y) value)))

(defun in-grid-bounds (grid pos)
  (destructuring-bind (x y) (coord-to-indices grid pos)
    (and (>= x 0) (>= y 0) (< x (array-dimension grid 0)) (< y (array-dimension grid 1)))))

(defun format-grid (grid &optional pos)
  (let ((strings '("")))
    (iter (for y below (array-dimension grid 1))
          (for s = (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
          (iter (for x below (array-dimension grid 0))
                (for c = (if (and pos (equalp (list x y) (coord-to-indices grid pos)))
                             #\D
                             (ecase (aref grid x y)
                               (unexplored #\Space)
                               (wall #\#)
                               (floor #\.)
                               (oxygen-system #\s)
                               (oxygen #\O))))
                (vector-push-extend c s))
          (push s strings))
    (push "" strings)
    (join *newline-str* strings)))

(defun reverse-dir (dir)
  (ecase dir
    (north 'south)
    (south 'north)
    (west 'east)
    (east 'west)))

(defun dir-to-cmd (dir)
  (ecase dir
    (north 1)
    (south 2)
    (west 3)
    (east 4)))

(defun move-pos (pos dir)
  (destructuring-bind (x y) pos
    (ecase dir
      (north (coord x (1+ y)))
      (south (coord x (1- y)))
      (west (coord (1- x) y))
      (east (coord (1+ x) y)))))

(defun path-to-oxygen (ints)
  (let ((comp (make-intcode-computer ints))
        (grid (make-array '(3 3) :element-type 'symbol :initial-element 'unexplored))
        (pos '(0 0))
        (path ())
        (sent-dir)
        (sent-reverse nil)
        (path-length-to-oxygen)
        (oxygen-pos))
    (setf (grid-aref grid pos) 'floor)
    (run-computer comp
                  :input-f (lambda ()
                             (setf sent-dir nil)
                             (iter (for dir in '(north south west east))
                                   (for new-pos = (move-pos pos dir))
                                   (if (not (in-grid-bounds grid new-pos)) (setf grid (expand-grid grid)))
                                   (when (eq (grid-aref grid new-pos) 'unexplored)
                                     (setf sent-dir dir)
                                     (setf sent-reverse nil)
                                     (finish)))
                             (when (not sent-dir)
                               (if (and (equalp pos '(0 0)) (not path))
                                   (return-from path-to-oxygen (values path-length-to-oxygen oxygen-pos grid))
                                   (progn (setf sent-dir (reverse-dir (first path)))
                                          (setf sent-reverse t))))
                             (dir-to-cmd sent-dir))

                  :output-f (lambda (x)
                              (let ((new-pos (move-pos pos sent-dir)))
                                (ecase x
                                  (0 (setf (grid-aref grid new-pos) 'wall))
                                  (1 (setf (grid-aref grid new-pos) 'floor)
                                   (setf pos new-pos)
                                   (if sent-reverse (pop path) (push sent-dir path)))
                                  (2 (setf (grid-aref grid new-pos) 'oxygen-system)
                                   (setf pos new-pos)
                                   (if sent-reverse (pop path) (push sent-dir path))
                                   (setf path-length-to-oxygen (length path))
                                   (setf oxygen-pos pos))))))))

(defun oxygen-fill-time (grid oxygen-pos)
  (let ((to-fill (list oxygen-pos))
        (minutes 0))
    (setf (grid-aref grid oxygen-pos) 'oxygen)
    (iter (for new-to-fill = ())
          (for filled = nil)
          (iter (for pos in to-fill)
                (iter (for dir in '(north south west east))
                      (for new-pos = (move-pos pos dir))
                      (when (ecase (grid-aref grid new-pos)
                              ((wall oxygen) nil)
                              (floor t))
                        (setf (grid-aref grid new-pos) 'oxygen)
                        (setf filled t)
                        (push new-pos new-to-fill))))
          (setf to-fill new-to-fill)
          (if filled (incf minutes))
          (while to-fill))
    minutes))
