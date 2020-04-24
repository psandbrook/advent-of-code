(defpackage aoc/2019/day24
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util))
(in-package :aoc/2019/day24)

;;; Part 1

(defun parse-state (str)
  (let ((result (make-array '(5 5) :initial-element nil)))
    (iter (for l in (str:lines str))
          (for y below (array-dimension result 0))
          (iter (for c in-string l)
                (for x below (array-dimension result 1))
                (setf (aref result y x) (ecase c
                                          (#\# t)
                                          (#\. nil)))))
    result))

(defun step-state (state)
  (let ((result (make-array (array-dimensions state) :initial-element nil)))
    (iter (for y below (array-dimension state 0))
          (iter (for x below (array-dimension state 1))
                (for adjacent = 0)
                (iter (for (adj-y adj-x) in (list (list (1- y) x)
                                                  (list (1+ y) x)
                                                  (list y (1- x))
                                                  (list y (1+ x))))
                      (if (and (array-in-bounds-p state adj-y adj-x)
                               (aref state adj-y adj-x))
                          (incf adjacent)))
                (setf (aref result y x) (if (aref state y x)
                                            (= adjacent 1)
                                            (or (= adjacent 1) (= adjacent 2))))))
    result))

(defun biodiversity-rating (state)
  (let ((result 0))
    (iter (for y below (array-dimension state 0))
          (with tile-value = 1)
          (iter (for x below (array-dimension state 1))
                (if (aref state y x) (incf result tile-value))
                (setf tile-value (* tile-value 2))))
    result))

(defun first-duplicate-rating (state)
  (let ((seen (make-hash-table :test #'equalp)))
    (setf (gethash state seen) t)
    (iter
      (setf state (step-state state))
      (if (gethash state seen) (leave (biodiversity-rating state)))
      (setf (gethash state seen) t))))

(deftest part-1-test
  (testing "first-duplicate-rating"
           (ok (= (first-duplicate-rating (parse-state (str:unlines (list "....#"
                                                                          "#..#."
                                                                          "#..##"
                                                                          "..#.."
                                                                          "#...."))))
                  2129920))))

;;; Part 2

(defun level-to-state (level)
  (let ((result (make-hash-table)))
    (setf (gethash 0 result) level)
    result))

(defun get-level (state level-i)
  (ensure-gethash level-i state (make-array '(5 5) :initial-element nil)))

(defun step-state-2 (state)
  (let ((result (make-hash-table))
        (start-level-i 0)
        (end-level-i 0))

    (iter (for (level-i nil) in-hashtable state)
          (minf start-level-i level-i)
          (maxf end-level-i level-i))

    (decf start-level-i)
    (incf end-level-i)

    (iter (for level-i from start-level-i to end-level-i)
          (for level = (get-level state level-i))
          (for result-level = (get-level result level-i))

          (iter (for y below (array-dimension level 0))
                (iter (for x below (array-dimension level 1))
                      (if (= y x 2) (next-iteration))

                      (for adjacent = 0)
                      (iter (for ((adj-y adj-x) dir) in (list (list (list (1- y) x) 'up)
                                                              (list (list (1+ y) x) 'down)
                                                              (list (list y (1- x)) 'left)
                                                              (list (list y (1+ x)) 'right)))
                            (cond ((= adj-y adj-x 2)
                                   (let ((sub-level (get-level state (1+ level-i))))
                                     (ecase dir
                                       ((up down)
                                        (let ((sub-y (ecase dir (up 4) (down 0))))
                                          (iter (for sub-x below 5)
                                                (if (aref sub-level sub-y sub-x) (incf adjacent)))))
                                       ((left right)
                                        (let ((sub-x (ecase dir (left 4) (right 0))))
                                          (iter (for sub-y below 5)
                                                (if (aref sub-level sub-y sub-x) (incf adjacent))))))))

                                  ((not (array-in-bounds-p level adj-y adj-x))
                                   (let ((super-level (get-level state (1- level-i))))
                                     (destructuring-bind (super-y super-x) (ecase dir
                                                                             (up '(1 2))
                                                                             (down '(3 2))
                                                                             (left '(2 1))
                                                                             (right '(2 3)))
                                       (if (aref super-level super-y super-x) (incf adjacent)))))

                                  (t (if (aref level adj-y adj-x) (incf adjacent)))))

                      (setf (aref result-level y x) (if (aref level y x)
                                                        (= adjacent 1)
                                                        (or (= adjacent 1) (= adjacent 2)))))))
    result))

(defun count-after-n (state n)
  (iter (repeat n)
        (setf state (step-state-2 state)))
  (let ((result 0))
    (iter (for (nil level) in-hashtable state)
          (iter (for y below (array-dimension level 0))
                (iter (for x below (array-dimension level 1))
                      (if (= y x 2) (next-iteration))
                      (if (aref level y x) (incf result)))))
    result))

(deftest part-2-test
  (testing "count-after-n"
           (ok (= (count-after-n (level-to-state (parse-state (str:unlines (list "....#"
                                                                                   "#..#."
                                                                                   "#..##"
                                                                                   "..#.."
                                                                                   "#...."))))
                                 10)
                  99))))
