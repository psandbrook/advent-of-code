(defpackage aoc/2019/day13
  (:use :cl
        :iterate
        :uiop
        :rove
        :aoc/util))
(in-package :aoc/2019/day13)

;;; Parts 1 & 2

(defparameter *wall-tile* 1)
(defparameter *block-tile* 2)
(defparameter *paddle-tile* 3)
(defparameter *ball-tile* 4)

(defun render-grid (grid)
  (iter (for y below (array-dimension grid 0))
        (for row-str = (make-array (array-dimension grid 1) :element-type 'character :fill-pointer 0))
        (iter (for x below (array-dimension grid 1))
              (vector-push (ecase (aref grid y x)
                             (0 #\Space)
                             (1 #\|)
                             (2 #\$)
                             (3 #\-)
                             (4 #\O))
                           row-str))
        (format t "~a~%" row-str)
        (setf row-str (make-array (array-dimension grid 1) :element-type 'character :fill-pointer 0))))

(defun find-pos (grid tile-id)
  (iter (for y below (array-dimension grid 0))
        (iter (for x below (array-dimension grid 1))
              (if (= (aref grid y x) tile-id) (return-from find-pos (coord x y))))))

(defun run-arcade-cabinet (intcode-prog &key play-for-free)
  (if play-for-free (setf (first intcode-prog) 2))
  (let ((comp (intcode:make-computer intcode-prog))
        (grid (make-array '(24 41) :element-type 'integer :initial-element 0))
        (score 0)
        (next-output-index 0)
        (next-tile-x)
        (next-tile-y))
    (intcode:run-computer comp
                          :input-f (lambda ()
                                     (let* ((paddle-pos (find-pos grid *paddle-tile*))
                                            (ball-pos (find-pos grid *ball-tile*))
                                            (paddle-x (coord-x paddle-pos))
                                            (ball-x (coord-x ball-pos)))
                                       (cond ((< paddle-x ball-x) 1)
                                             ((> paddle-x ball-x) -1)
                                             (t 0))))
                          :output-f (lambda (val)
                                      (ecase next-output-index
                                        (0 (setf next-tile-x val)
                                         (incf next-output-index))
                                        (1 (setf next-tile-y val)
                                         (incf next-output-index))
                                        (2 (if (and (= next-tile-x -1) (= next-tile-y 0))
                                               (progn (setf score val)
                                                      (format t "SCORE: ~a~%" score))
                                               (setf (aref grid next-tile-y next-tile-x) val))
                                         (setf next-output-index 0)))))
    (let ((block-tiles (iter (for y below (array-dimension grid 0))
                             (sum (iter (for x below (array-dimension grid 1))
                                        (counting (= (aref grid y x) *block-tile*)))))))
      (render-grid grid)
      (list block-tiles score))))
