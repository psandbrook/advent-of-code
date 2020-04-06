(defpackage aoc/2019/day9
  (:use :cl
        :uiop
        :rove))
(in-package :aoc/2019/day9)

;;; Part 1

(defclass intcode-computer ()
  ((mem :initarg :mem :accessor mem)
   (ip :initform 0 :accessor ip)
   (relative-base :initform 0 :accessor relative-base)))

(defun parse-intcode-str (str)
  (mapcar #'parse-integer (split-string str :separator ",")))

(defun make-intcode-computer (ints)
  (make-instance 'intcode-computer :mem (make-array (length ints)
                                                    :element-type 'integer
                                                    :initial-contents ints
                                                    :adjustable t)))

(defun expand-mem-to (comp pos)
  (if (>= pos (length (mem comp))) (adjust-array (mem comp) (1+ pos) :initial-element 0)))

(defun get-pos (comp pos)
  (expand-mem-to comp pos)
  (elt (mem comp) pos))

(defun get-indirect-pos (comp pos &key relative)
  (let ((pos-2 (get-pos comp pos)))
    (if relative (incf pos-2 (relative-base comp)))
    (get-pos comp pos-2)))

(defun (setf get-indirect-pos) (value comp pos &key relative)
  (let ((pos-2 (get-pos comp pos)))
    (if relative (incf pos-2 (relative-base comp)))
    (expand-mem-to comp pos-2)
    (setf (elt (mem comp) pos-2) value)))

(defun run-computer (comp)
  (with-accessors ((ip ip) (relative-base relative-base)) comp
    (loop
      for ip-value-str = (write-to-string (get-pos comp ip))
      for opcode-start = (max (- (length ip-value-str) 2) 0)
      for opcode = (parse-integer ip-value-str :start opcode-start)
      for param-modes = (nreverse (map 'list #'digit-char-p (subseq ip-value-str 0 opcode-start)))
      do (flet ((get-param (index)
                  (let ((pos (+ ip 1 index))
                        (mode (if (>= index (length param-modes)) 0 (nth index param-modes))))
                    (ecase mode
                      (0 (get-indirect-pos comp pos))
                      (1 (get-pos comp pos))
                      (2 (get-indirect-pos comp pos :relative t)))))
                ((setf get-param) (value index)
                  (let ((pos (+ ip 1 index))
                        (mode (if (>= index (length param-modes)) 0 (nth index param-modes))))
                    (ecase mode
                      (0 (setf (get-indirect-pos comp pos) value))
                      (2 (setf (get-indirect-pos comp pos :relative t) value))))))
           (ecase opcode
             (1 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (setf (get-param 2) (+ p0 p1))
                  (incf ip 4)))
             (2 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (setf (get-param 2) (* p0 p1))
                  (incf ip 4)))
             (3 (format t "INPUT: ")
              (let ((input (read)))
                (setf (get-param 0) input)
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
                  (setf (get-param 2) (if (< p0 p1) 1 0))
                  (incf ip 4)))
             (8 (let ((p0 (get-param 0))
                      (p1 (get-param 1)))
                  (setf (get-param 2) (if (= p0 p1) 1 0))
                  (incf ip 4)))
             (9 (let ((p0 (get-param 0)))
                  (incf relative-base p0)
                  (incf ip 2)))
             (99 (return)))))))

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
