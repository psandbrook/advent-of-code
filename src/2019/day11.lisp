(defpackage aoc/2019/day11
  (:use :cl
        :uiop
        :rove
        :aoc/util
        :aoc/2019/intcode))
(in-package :aoc/2019/day11)

;;; Parts 1 & 2

(defun run-painting-robot (intcode-prog &key start-white)
  (let ((comp (make-intcode-computer intcode-prog))
        (grid (make-array '(140 140) :initial-element 0 :element-type 'integer))
        (robot-pos (list 50 50))
        (robot-dir 'up)
        (coords-painted ())
        (next-output-index 0))
    (macrolet ((get-grid-at-robot () '(aref grid (coord-x robot-pos) (coord-y robot-pos))))
      (if start-white (setf (get-grid-at-robot) 1))
      (run-computer comp
                    :input-f (lambda () (get-grid-at-robot))
                    :output-f (lambda (x)
                                (ecase next-output-index
                                  (0 (setf (get-grid-at-robot) x)
                                   (pushnew (copy-list robot-pos) coords-painted :test #'equalp)
                                   (incf next-output-index))
                                  (1 (setf robot-dir (ecase x
                                                       (0 (ecase robot-dir
                                                            (up 'left)
                                                            (right 'up)
                                                            (down 'right)
                                                            (left 'down)))
                                                       (1 (ecase robot-dir
                                                            (up 'right)
                                                            (right 'down)
                                                            (down 'left)
                                                            (left 'up)))))
                                   (ecase robot-dir
                                     (up (incf (cadr robot-pos)))
                                     (right (incf (car robot-pos)))
                                     (down (decf (cadr robot-pos)))
                                     (left (decf (car robot-pos))))
                                   (setf next-output-index 0)))))
      (list (length coords-painted) grid))))

(defun grid-to-strings (grid)
  (loop
    with out = ()
    with s = (make-array 200 :element-type 'character :fill-pointer 0 :adjustable t)
    for y below (array-dimension grid 1)
    do (loop
         for x below (array-dimension grid 0)
         do (vector-push-extend (digit-char (aref grid x y)) s))
    (push s out)
    (setf s (make-array 200 :element-type 'character :fill-pointer 0 :adjustable t))
    finally (return out)))

(deftest part-1-test
  (testing "run-computer"
           (flet ((check-intcode-prog (str out)
                    (let ((comp (make-intcode-computer (parse-intcode-str str))))
                      (run-computer comp)
                      (equalp (mem comp) (mem (make-intcode-computer (parse-intcode-str out)))))))
             (ok (check-intcode-prog "1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"))
             (ok (check-intcode-prog "1,0,0,0,99" "2,0,0,0,99"))
             (ok (check-intcode-prog "2,3,0,3,99" "2,3,0,6,99"))
             (ok (check-intcode-prog "2,4,4,5,99,0" "2,4,4,5,99,9801"))
             (ok (check-intcode-prog "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99")))))
