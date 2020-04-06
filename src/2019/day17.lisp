(defpackage aoc/2019/day17
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util
        :aoc/2019/intcode)
  (:import-from :str :concat :join :trim))
(in-package :aoc/2019/day17)

;;; Parts 1 & 2

(defun format-map (map)
  (iter (with strings = (list ""))
        (for y below (array-dimension map 0))
        (for line = (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (iter (for x below (array-dimension map 1))
              (vector-push-extend (ecase (aref map y x)
                                    (scaffold #\#)
                                    (space #\.)
                                    (robot-up #\^)
                                    (robot-down #\v)
                                    (robot-left #\<)
                                    (robot-right #\>)
                                    (robot-fall #\X))
                                  line))
        (push line strings)
        (finally (push "" strings)
                 (return (join *newline-str* (nreverse strings))))))

(defun get-map (ints)
  (let ((comp (make-intcode-computer ints))
        (out-line (make-array 0 :adjustable t :fill-pointer 0))
        (out-vector (make-array 0 :adjustable t :fill-pointer 0)))
    (run-computer comp :output-f (lambda (x)
                                   (if (= x 10)
                                       (progn (if (> (length out-line) 0) (vector-push-extend out-line out-vector))
                                              (setf out-line (make-array 0 :adjustable t :fill-pointer 0)))
                                       (vector-push-extend (ecase x
                                                             (35 'scaffold)
                                                             (46 'space)
                                                             (94 'robot-up)
                                                             (118 'robot-down)
                                                             (60 'robot-left)
                                                             (62 'robot-right)
                                                             (88 'robot-fall))
                                                           out-line))))
    (iter (with out-array = (make-array (list (length out-vector) (length (elt out-vector 0)))))
          (for y below (array-dimension out-array 0))
          (iter (for x below (array-dimension out-array 1))
                (setf (aref out-array y x) (elt (elt out-vector y) x)))
          (finally (return out-array)))))

(defun camera-calibration-value (map)
  (iter (with out = 0)
        (for y below (array-dimension map 0))
        (iter (for x below (array-dimension map 1))
              (if (and (> y 0) (< y (1- (array-dimension map 0)))
                       (> x 0) (< x (1- (array-dimension map 1)))
                       (eq (aref map y x) 'scaffold)
                       (eq (aref map (1- y) x) 'scaffold)
                       (eq (aref map (1+ y) x) 'scaffold)
                       (eq (aref map y (1- x)) 'scaffold)
                       (eq (aref map y (1+ x)) 'scaffold))
                  (incf out (* x y))))
        (finally (return out))))

(defun in-bounds (array pos)
  (destructuring-bind (len-y len-x) (array-dimensions array)
    (destructuring-bind (y x) pos
      (and (>= y 0)
           (< y len-y)
           (>= x 0)
           (< x len-x)))))

(defun apply-dir (coord dir &optional (n 1))
  (destructuring-bind (y x) coord
    (ecase dir
      (up (list (- y n) x))
      (down (list (+ y n) x))
      (left (list y (- x n)))
      (right (list y (+ x n))))))

(defun turn-dir (dir turn)
  (ecase dir
    (up (ecase turn
          (left 'left)
          (right 'right)))
    (down (ecase turn
            (left 'right)
            (right 'left)))
    (left (ecase turn
            (left 'down)
            (right 'up)))
    (right (ecase turn
             (left 'up)
             (right 'down)))))

(defun get-one-path (map)
  (let ((path ()) bot-pos bot-dir)
    (iter outer (for y below (array-dimension map 0))
          (iter (for x below (array-dimension map 1))
                (case (aref map y x)
                  ((robot-up robot-down robot-left robot-right)
                   (setf bot-pos (list y x))
                   (setf bot-dir (ecase (aref map y x)
                                   (robot-up 'up)
                                   (robot-down 'down)
                                   (robot-left 'left)
                                   (robot-right 'right)))
                   (return-from outer)))))
    (labels ((forward-step (pos dir)
               (iter (with n = 0)
                     (for front = (apply-dir pos dir))
                     (if (and (in-bounds map front) (eq (apply #'aref map front) 'scaffold))
                         (progn (incf n)
                                (setf pos front))
                         (leave (list n pos)))))
             (do-next-step ()
               (let ((front (apply-dir bot-pos bot-dir)))
                 (assert (or (not (in-bounds map front))
                             (eq (apply #'aref map front) 'space)))
                 (let* ((left-dir (turn-dir bot-dir 'left))
                        (right-dir (turn-dir bot-dir 'right))
                        (left (apply-dir bot-pos left-dir))
                        (right (apply-dir bot-pos right-dir)))
                   (cond ((and (in-bounds map left) (eq (apply #'aref map left) 'scaffold))
                          (push 'left path)
                          (destructuring-bind (n new-bot-pos) (forward-step bot-pos left-dir)
                            (push n path)
                            (setf bot-pos new-bot-pos)
                            (setf bot-dir left-dir))
                          t)
                         ((and (in-bounds map right) (eq (apply #'aref map right) 'scaffold))
                          (push 'right path)
                          (destructuring-bind (n new-bot-pos) (forward-step bot-pos right-dir)
                            (push n path)
                            (setf bot-pos new-bot-pos)
                            (setf bot-dir right-dir))
                          t)
                         (t nil))))))
      (iter
        (if (not (do-next-step)) (leave (nreverse path)))))))

(defun get-all-fun-lengths ()
  (let ((all-init-lengths ())
        (all-lengths ()))
    (map-combinations (lambda (c) (push c all-init-lengths))
                      (iter (for i from 1 to 11) (collect i) (collect i) (collect i))
                      :length 3)
    (setf all-init-lengths (delete-duplicates (nreverse all-init-lengths) :test #'equalp))
    (iter (for lengths in all-init-lengths)
          (map-permutations (lambda (p) (push p all-lengths)) lengths))
    (delete-duplicates (nreverse all-lengths) :test #'equalp)))

(defun movement-fun-to-ints (move-fun)
  (macrolet ((collect-char (c) `(collect (char-code ,c)))
             (collect-comma () '(collect-char #\,)))
    (let ((ints (iter outer (for e in move-fun)
                      (case e
                        (a (collect-char #\A)
                           (collect-comma))
                        (b (collect-char #\B)
                           (collect-comma))
                        (c (collect-char #\C)
                           (collect-comma))
                        (left (collect-char #\L)
                              (collect-comma))
                        (right (collect-char #\R)
                               (collect-comma))
                        (otherwise (iter (for c in-string (write-to-string e))
                                         (in outer (collect-char c)))
                                   (collect-comma))))))
      (setf (last-elt ints) (char-code #\Newline))
      ints)))

(defun movement-instructions (path)
  (iter outer (for (a-len b-len c-len) in (get-all-fun-lengths))
        (for a-fun = (subseq path 0 a-len))
        (for rest-path = (subseq path a-len))
        (for main-routine = (list 'a))
        (flet ((check-fun (fun sym)
                 (if (starts-with-subseq fun rest-path)
                     (progn (push sym main-routine)
                            (setf rest-path (subseq rest-path (length fun)))
                            t)
                     nil)))
          (iter
            (if (not (check-fun a-fun 'a)) (finish)))

          (for b-fun = (subseq rest-path 0 b-len))
          (push 'b main-routine)
          (setf rest-path (subseq rest-path b-len))
          (iter
            (if (not (check-fun b-fun 'b)) (finish)))

          (for c-fun = (subseq rest-path 0 c-len))
          (push 'c main-routine)
          (setf rest-path (subseq rest-path c-len))
          (iter
            (if (not (check-fun c-fun 'c)) (finish)))

          (iter
            (if (emptyp rest-path) (return-from outer (list (nreverse main-routine) a-fun b-fun c-fun)))
            (if (not (or (check-fun a-fun 'a) (check-fun b-fun 'b) (check-fun c-fun 'c)))
                (finish))))))

(defun execute-movement-instructions (ints instrs)
  (let ((ints (copy-list ints)))
    (setf (first ints) 2)
    (destructuring-bind (main-routine a-fun b-fun c-fun) instrs
      (let ((main-routine-ints (movement-fun-to-ints main-routine))
            (a-fun-ints (movement-fun-to-ints a-fun))
            (b-fun-ints (movement-fun-to-ints b-fun))
            (c-fun-ints (movement-fun-to-ints c-fun))
            (feed-ints (mapcar #'char-code (list #\n #\Newline))))
        (assert (and (<= (length main-routine-ints) 20)
                     (<= (length a-fun-ints) 20)
                     (<= (length b-fun-ints) 20)
                     (<= (length c-fun-ints) 20)))
        (run-computer (make-intcode-computer ints)
                      :input-f (lambda ()
                                 (if main-routine-ints
                                     (pop main-routine-ints)
                                     (if a-fun-ints
                                         (pop a-fun-ints)
                                         (if b-fun-ints
                                             (pop b-fun-ints)
                                             (if c-fun-ints
                                                 (pop c-fun-ints)
                                                 (pop feed-ints)))))))))))
