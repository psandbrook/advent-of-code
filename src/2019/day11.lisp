(defpackage aoc/2019/day11
  (:use :cl
        :uiop
        :rove
        :aoc/util))
(in-package :aoc/2019/day11)

;;; Parts 1 & 2

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

(defun run-computer (comp &key
                          (input-f #'(lambda () (format t "INPUT: ") (read)))
                          (output-f #'(lambda (x) (format t "OUTPUT: ~a~%" x))))
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
             (3 (let ((input (funcall input-f)))
                  (setf (get-param 0) input)
                  (incf ip 2)))
             (4 (let ((p0 (get-param 0)))
                  (funcall output-f p0)
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
