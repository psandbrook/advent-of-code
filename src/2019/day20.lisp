(defpackage aoc/2019/day20
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util))
(in-package :aoc/2019/day20)

;;; Parts 1 & 2

(defclass portal-tile ()
  ((label :initarg :label :accessor label)
   (destination :initarg :destination :accessor destination)
   (outer :initarg :outer :accessor outer)))

(defun make-portal-tile (label destination outer)
  (make-instance 'portal-tile :label label :destination destination :outer outer))

(defmethod print-object ((obj portal-tile) stream)
  (with-accessors ((label label) (destination destination) (outer outer)) obj
    (print-unreadable-object (obj stream :type t)
      (format stream "~a ~a ~a" label destination outer))))

(defun parse-map (str)
  (let* ((lines (str:lines str))
         (map (make-array (list (length lines) (length (first lines))) :initial-element nil)))

    (iter (for line in lines)
      (for y below (array-dimension map 0))
      (iter (for c in-string line)
        (for x below (array-dimension map 1))
        (setf (aref map y x) (case c
                               (#\Space nil)
                               (#\# 'wall)
                               (#\. 'passage)
                               (otherwise (assert (upper-case-p c))
                                          c)))))

    (iter (for y below (array-dimension map 0))
      (iter (for x below (array-dimension map 1))
        (when (typep (aref map y x) 'character)
          (iter (for (passage-pos other-char-pos) in (list (list (list (+ y 2) x) (list (1+ y) x))
                                                           (list (list (1- y) x) (list (1+ y) x))
                                                           (list (list y (1- x)) (list y (1+ x)))
                                                           (list (list y (+ x 2)) (list y (1+ x)))))
            (when (and (apply #'array-in-bounds-p map passage-pos)
                       (eq (apply #'aref map passage-pos) 'passage))
              (let ((label (coerce (list (aref map y x) (apply #'aref map other-char-pos))
                                   'string)))
                (setf (aref map y x) nil
                      (apply #'aref map other-char-pos) nil
                      (apply #'aref map passage-pos) label)
                (else (error "Could not find passage."))
                (finish)))))))

    (flet ((is-outer (pos)
             (destructuring-bind (y x) pos
               (or (= y 2)
                   (= y (- (array-dimension map 0) 3))
                   (= x 2)
                   (= x (- (array-dimension map 1) 3))))))
      (iter (for y below (array-dimension map 0))
        (iter (for x below (array-dimension map 1))
          (for tile = (aref map y x))
          (when (typep tile 'string)
            (cond ((equal tile "AA") (setf (aref map y x) 'start))
                  ((equal tile "ZZ") (setf (aref map y x) 'end))
                  (t
                   (iter (for y-2 below (array-dimension map 0))
                     (iter (for x-2 below (array-dimension map 1))
                       (for tile-2 = (aref map y-2 x-2))
                       (when (and (/= y y-2) (/= x x-2) (equal tile tile-2))
                         (setf (aref map y x) (make-portal-tile
                                                tile
                                                (list y-2 x-2)
                                                (is-outer (list y x)))
                               (aref map y-2 x-2) (make-portal-tile
                                                    tile
                                                    (list y x)
                                                    (is-outer (list y-2 x-2)))))))))))))

    map))

(defun traversable (tile)
  (or (member tile '(passage start end))
      (typep tile 'portal-tile)))

(defun shortest-path (map)
  (let ((start-pos (iter outer (for y below (array-dimension map 0))
                     (iter (for x below (array-dimension map 1))
                       (if (eq (aref map y x) 'start) (return-from outer (list y x))))))
        (discovered (make-array (array-dimensions map) :initial-element nil))
        (q ()))
    (setf (apply #'aref discovered start-pos) t)
    (push (list start-pos 0) q)
    (iter (while (not (emptyp q)))
      (for (pos distance) = (rpop q))
      (for tile = (apply #'aref map pos))
      (when (eq tile 'end)
        (return-from shortest-path distance))

      (for (pos-y pos-x) = pos)
      (for neighbours = (list (list (1- pos-y) pos-x)
                              (list (1+ pos-y) pos-x)
                              (list pos-y (1- pos-x))
                              (list pos-y (1+ pos-x))))
      (when (typep tile 'portal-tile)
        (push (destination tile) neighbours))
      (iter (for n in neighbours)
        (when (and (apply #'array-in-bounds-p map n)
                   (traversable (apply #'aref map n))
                   (not (apply #'aref discovered n)))
          (setf (apply #'aref discovered n) t)
          (push (list n (1+ distance)) q))))))

(deftest part-1-test
  (testing "shortest-path"
    (ok (= (shortest-path (parse-map (str:unlines (list "         A           "
                                                        "         A           "
                                                        "  #######.#########  "
                                                        "  #######.........#  "
                                                        "  #######.#######.#  "
                                                        "  #######.#######.#  "
                                                        "  #######.#######.#  "
                                                        "  #####  B    ###.#  "
                                                        "BC...##  C    ###.#  "
                                                        "  ##.##       ###.#  "
                                                        "  ##...DE  F  ###.#  "
                                                        "  #####    G  ###.#  "
                                                        "  #########.#####.#  "
                                                        "DE..#######...###.#  "
                                                        "  #.#########.###.#  "
                                                        "FG..#########.....#  "
                                                        "  ###########.#####  "
                                                        "             Z       "
                                                        "             Z       "))))
           23))

    (ok (= (shortest-path (parse-map (str:unlines (list "                   A               "
                                                        "                   A               "
                                                        "  #################.#############  "
                                                        "  #.#...#...................#.#.#  "
                                                        "  #.#.#.###.###.###.#########.#.#  "
                                                        "  #.#.#.......#...#.....#.#.#...#  "
                                                        "  #.#########.###.#####.#.#.###.#  "
                                                        "  #.............#.#.....#.......#  "
                                                        "  ###.###########.###.#####.#.#.#  "
                                                        "  #.....#        A   C    #.#.#.#  "
                                                        "  #######        S   P    #####.#  "
                                                        "  #.#...#                 #......VT"
                                                        "  #.#.#.#                 #.#####  "
                                                        "  #...#.#               YN....#.#  "
                                                        "  #.###.#                 #####.#  "
                                                        "DI....#.#                 #.....#  "
                                                        "  #####.#                 #.###.#  "
                                                        "ZZ......#               QG....#..AS"
                                                        "  ###.###                 #######  "
                                                        "JO..#.#.#                 #.....#  "
                                                        "  #.#.#.#                 ###.#.#  "
                                                        "  #...#..DI             BU....#..LF"
                                                        "  #####.#                 #.#####  "
                                                        "YN......#               VT..#....QG"
                                                        "  #.###.#                 #.###.#  "
                                                        "  #.#...#                 #.....#  "
                                                        "  ###.###    J L     J    #.#.###  "
                                                        "  #.....#    O F     P    #.#...#  "
                                                        "  #.###.#####.#.#####.#####.###.#  "
                                                        "  #...#.#.#...#.....#.....#.#...#  "
                                                        "  #.#####.###.###.#.#.#########.#  "
                                                        "  #...#.#.....#...#.#.#.#.....#.#  "
                                                        "  #.###.#####.###.###.#.#.#######  "
                                                        "  #.#.........#...#.............#  "
                                                        "  #########.###.###.#############  "
                                                        "           B   J   C               "
                                                        "           U   P   P               "))))
           58))))

(defun shortest-path-2 (map)
  (let ((start-pos (iter outer (for y below (array-dimension map 0))
                     (iter (for x below (array-dimension map 1))
                       (if (eq (aref map y x) 'start) (return-from outer (list y x))))))
        (discovered (make-hash-table))
        (q ()))
    (flet ((set-discovered (pos level)
             (setf (apply #'aref (ensure-gethash level
                                                 discovered
                                                 (make-array (array-dimensions map) :initial-element nil))
                          pos)
                   t))
           (get-discovered (pos level)
             (apply #'aref (ensure-gethash level
                                           discovered
                                           (make-array (array-dimensions map) :initial-element nil))
                    pos)))
      (set-discovered start-pos 0)
      (push (list start-pos 0 0) q)
      (iter (while (not (emptyp q)))
        (for (pos level distance) = (rpop q))
        (for tile = (apply #'aref map pos))
        (when (and (eq tile 'end) (= level 0))
          (return-from shortest-path-2 distance))

        (for (pos-y pos-x) = pos)
        (for neighbours = (list (list (list (1- pos-y) pos-x) level)
                                (list (list (1+ pos-y) pos-x) level)
                                (list (list pos-y (1- pos-x)) level)
                                (list (list pos-y (1+ pos-x)) level)))
        (when (and (typep tile 'portal-tile)
                   (or (>= level 1)
                       (not (outer tile))))
          (let ((new-level (if (outer tile) (1- level) (1+ level))))
            (push (list (destination tile) new-level) neighbours)))
        (iter (for (n-pos n-level) in neighbours)
          (when (and (apply #'array-in-bounds-p map n-pos)
                     (traversable (apply #'aref map n-pos))
                     (not (get-discovered n-pos n-level)))
            (set-discovered n-pos n-level)
            (push (list n-pos n-level (1+ distance)) q)))))))

(deftest part-2-test
  (testing "shortest-path-2"
    (ok (= (shortest-path-2 (parse-map (str:unlines (list "             Z L X W       C                 "
                                                          "             Z P Q B       K                 "
                                                          "  ###########.#.#.#.#######.###############  "
                                                          "  #...#.......#.#.......#.#.......#.#.#...#  "
                                                          "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
                                                          "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
                                                          "  #.###.#######.###.###.#.###.###.#.#######  "
                                                          "  #...#.......#.#...#...#.............#...#  "
                                                          "  #.#########.#######.#.#######.#######.###  "
                                                          "  #...#.#    F       R I       Z    #.#.#.#  "
                                                          "  #.###.#    D       E C       H    #.#.#.#  "
                                                          "  #.#...#                           #...#.#  "
                                                          "  #.###.#                           #.###.#  "
                                                          "  #.#....OA                       WB..#.#..ZH"
                                                          "  #.###.#                           #.#.#.#  "
                                                          "CJ......#                           #.....#  "
                                                          "  #######                           #######  "
                                                          "  #.#....CK                         #......IC"
                                                          "  #.###.#                           #.###.#  "
                                                          "  #.....#                           #...#.#  "
                                                          "  ###.###                           #.#.#.#  "
                                                          "XF....#.#                         RF..#.#.#  "
                                                          "  #####.#                           #######  "
                                                          "  #......CJ                       NM..#...#  "
                                                          "  ###.#.#                           #.###.#  "
                                                          "RE....#.#                           #......RF"
                                                          "  ###.###        X   X       L      #.#.#.#  "
                                                          "  #.....#        F   Q       P      #.#.#.#  "
                                                          "  ###.###########.###.#######.#########.###  "
                                                          "  #.....#...#.....#.......#...#.....#.#...#  "
                                                          "  #####.#.###.#######.#######.###.###.#.#.#  "
                                                          "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
                                                          "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
                                                          "  #.......#.....#.#...#...............#...#  "
                                                          "  #############.#.#.###.###################  "
                                                          "               A O F   N                     "
                                                          "               A A D   M                     "))))
           396))))
