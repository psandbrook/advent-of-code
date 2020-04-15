(defpackage aoc/2019/day19
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util
        :aoc/2019/intcode))
(in-package :aoc/2019/day19)

;;; Part 1

(defun tractor-points (ints)
  (let ((result 0))
    (iter (for y below 50)
          (iter (for x below 50)
                (for input = (list x y))
                (run-computer (make-intcode-computer ints)
                              :input-f (lambda () (pop input))
                              :output-f (lambda (x) (incf result x)))))
    result))

;;; Part 2

(defun look-up-point (ints point)
  (destructuring-bind (y x) point
    (let ((input (list x y))
          output)
      (run-computer (make-intcode-computer ints)
                    :input-f (lambda () (pop input))
                    :output-f (lambda (a) (setf output a)))
      (= output 1))))

(defun get-line-length (ints y)
  (iter (for x from 0)
        (with prev = nil)
        (for val = (look-up-point ints (list y x)))
        (counting val)
        (if (and prev (not val)) (finish))
        (setf prev val)))

(defun tractor-map (ints size &key (offset '(0 0)))
  (let ((map (make-array size :element-type 'boolean :initial-element nil)))
    (destructuring-bind (y-offset x-offset) offset
      (lparallel:pdotimes (y-base (array-dimension map 0))
                          (let ((y (+ y-base y-offset)))
                            (iter (for x-base below (array-dimension map 1))
                                  (for x = (+ x-base x-offset))
                                  (setf (aref map y-base x-base) (look-up-point ints (list y x)))))))
    map))

(defun format-tractor-map (map)
  (let ((result (list "")))
    (iter (for y below (array-dimension map 0))
          (for line = (extendable-string))
          (iter (for x below (array-dimension map 1))
                (vector-push-extend (if (aref map y x) #\# #\.) line))
          (push line result))
    (push "" result)
    (str:join *newline-str* (nreverse result))))

(defun find-line-with-length (ints length)
  (iter
    (with lower = (* length 5))
    (with upper = (* length 20))
    (for y = (round (+ lower upper) 2))
    (for y-length = (get-line-length ints y))
    (cond ((< y-length length) (setf lower y))
          ((= y-length length) (leave y))
          (t (setf upper y)))))

(defun find-start-of-line (ints y)
  (iter (for x from 0)
        (if (look-up-point ints (list y x))
            (leave x))))

(defun check-square-fit (map length point)
  (destructuring-bind (y x) point
    (and (aref map y x)
         (aref map (1- (+ y length)) x)
         (aref map y (1- (+ x length)))
         (aref map (1- (+ y length)) (1- (+ x length))))))

(defun real-check-square-fit (ints length point)
  (destructuring-bind (y x) point
    (and (look-up-point ints point)
         (look-up-point ints (list (1- (+ y length)) x))
         (look-up-point ints (list y (1- (+ x length))))
         (look-up-point ints (list (1- (+ y length)) (1- (+ x length)))))))

(defun find-square-fit (ints length map-size offset)
  (destructuring-bind (offset-y offset-x) offset
    (let ((map (tractor-map ints map-size :offset offset)))
      (iter (for y below (array-dimension map 0))
            (for y-base from offset-y)
            (iter (for x below (array-dimension map 1))
                  (for x-base from offset-x)
                  (if (check-square-fit map length (list y x))
                      (return-from find-square-fit (list y-base x-base))))))))
