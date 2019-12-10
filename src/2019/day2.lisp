(defpackage aoc/2019/day2
  (:use :cl
        :uiop
        :rove))
(in-package :aoc/2019/day2)

;;; Part 1

(defun run-intcode (mem)
  (macrolet ((deref (pos) `(elt mem (elt mem ,pos))))
    (loop
      with pc = 0
      for opcode = (elt mem pc)
      do (ccase opcode
           (1 (let ((a1 (deref (+ pc 1)))
                    (a2 (deref (+ pc 2))))
              (setf (deref (+ pc 3)) (+ a1 a2))))
           (2 (let ((a1 (deref (+ pc 1)))
                    (a2 (deref (+ pc 2))))
              (setf (deref (+ pc 3)) (* a1 a2))))
           (99 (return)))
         (incf pc 4))))

(defun parse-intcode-str (str)
  (map 'vector #'parse-integer (split-string str :separator ",")))

(defun restore-1202-state (mem)
  (setf (elt mem 1) 12)
  (setf (elt mem 2) 2))

(deftest part-1-test
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

;;; Part 2

(defun run-intcode-io (mem noun verb)
  (setf (elt mem 1) noun)
  (setf (elt mem 2) verb)
  (run-intcode mem)
  (elt mem 0))

(defun find-part-2-answer (input-mem)
  (loop named outer
    for noun from 0 to 99
    do (loop
         for verb from 0 to 99
         if (= (run-intcode-io (copy-seq input-mem) noun verb) 19690720)
           do (return-from outer (+ (* 100 noun) verb)))))
