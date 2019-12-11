(defpackage aoc/2019/day5
  (:use :cl
        :uiop
        :rove))
(in-package :aoc/2019/day5)

;;; Parts 1 & 2

(defun run-intcode (mem)
  (macrolet ((deref (pos) `(elt mem (elt mem ,pos))))
    (loop
      with ip = 0
      for ip-value-str = (write-to-string (elt mem ip))
      for opcode-start = (max (- (length ip-value-str) 2) 0)
      for opcode = (parse-integer ip-value-str :start opcode-start)
      for param-modes = (nreverse (map 'list #'digit-char-p (subseq ip-value-str 0 opcode-start)))
      do (flet ((get-param (index)
                  (let ((pos (+ ip 1 index))
                        (mode (if (>= index (length param-modes)) 0 (nth index param-modes))))
                    (ecase mode
                      (0 (deref pos))
                      (1 (elt mem pos))))))
           (ecase opcode
             (1 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (setf (deref (+ ip 3)) (+ p0 p1))
                  (incf ip 4)))
             (2 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (setf (deref (+ ip 3)) (* p0 p1))
                  (incf ip 4)))
             (3 (format t "INPUT: ")
                (let ((input (read)))
                  (setf (deref (+ ip 1)) input)
                  (incf ip 2)))
             (4 (let ((p0 (get-param 0)))
                  (format t "OUTPUT: ~a~%" p0)
                  (incf ip 2)))
             (5 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (if (/= p0 0) (setf ip p1) (incf ip 3))))
             (6 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (if (= p0 0) (setf ip p1) (incf ip 3))))
             (7 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (setf (deref (+ ip 3)) (if (< p0 p1) 1 0))
                  (incf ip 4)))
             (8 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (setf (deref (+ ip 3)) (if (= p0 p1) 1 0))
                  (incf ip 4)))
             (99 (return)))))))

(defun parse-intcode-str (str)
  (map 'vector #'parse-integer (split-string str :separator ",")))

(deftest day5-test
  (testing "run-intcode"
    (flet ((check-intcode-prog (str out)
             (let ((mem (parse-intcode-str str)))
               (run-intcode mem)
               (equalp mem (parse-intcode-str out)))))
      (ok (check-intcode-prog "1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"))
      (ok (check-intcode-prog "1,0,0,0,99" "2,0,0,0,99"))
      (ok (check-intcode-prog "2,3,0,3,99" "2,3,0,6,99"))
      (ok (check-intcode-prog "2,4,4,5,99,0" "2,4,4,5,99,9801"))
      (ok (check-intcode-prog "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99")))))
