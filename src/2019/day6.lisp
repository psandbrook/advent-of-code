(defpackage aoc/2019/day6
  (:use :cl
        :uiop
        :rove))
(in-package :aoc/2019/day6)

;;; Part 1

(defun parse-orbit-map (str)
  (mapcan #'(lambda (s) (if (/= (length s) 0) (list (split-string s :separator ")"))))
  (split-string str :separator (coerce '(#\Newline #\Space) 'string))))

(defun make-orbit-tree (orbit-map)
  (loop
    with tree = (make-hash-table :test #'equal)
    for (parent child) in orbit-map
    do (if (gethash parent tree) (push child (gethash parent tree)) (setf (gethash parent tree) (list child)))
    (if (not (gethash child tree)) (setf (gethash child tree) ()))
    finally (return tree)))

(defun total-orbits (orbit-tree)
  (let ((depth -1)
        (total-depth 0))
    (labels ((visit-node (name)
               (incf depth)
               (loop
                 for child in (gethash name orbit-tree)
                 do (visit-node child)
                 finally (incf total-depth depth))
               (decf depth)))
      (visit-node "COM")
      total-depth)))

(deftest part-1-test
  (testing "total-orbits"
           (ok (= (total-orbits (make-orbit-tree (parse-orbit-map "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L")))
                  42))))

;;; Part 2

(defun make-orbit-graph (orbit-map)
  (loop
    with graph = (make-hash-table :test #'equal)
    for (parent child) in orbit-map
    do (if (gethash parent graph) (push child (gethash parent graph)) (setf (gethash parent graph) (list child)))
    (if (gethash child graph) (push parent (gethash child graph)) (setf (gethash child graph) (list parent)))
    finally (return graph)))

(defun transfers-to-san (orbit-graph)
  (let ((start-node (first (gethash "YOU" orbit-graph)))
        (end-node (first (gethash "SAN" orbit-graph)))
        (transfers -1))
    (labels ((visit-node (parent name)
               (incf transfers)
               (if (equal name end-node) (return-from visit-node t))
               (loop
                 for node in (gethash name orbit-graph)
                 if (not (equal node parent)) do (if (visit-node name node) (return-from visit-node t)))
               (decf transfers)
               nil))
      (visit-node "YOU" start-node)
      transfers)))

(deftest part-2-test
  (testing "transfers-to-san"
           (ok (= (transfers-to-san
                    (make-orbit-graph (parse-orbit-map "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L K)YOU I)SAN")))
                  4))))
