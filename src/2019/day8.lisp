(defpackage aoc/2019/day8
  (:use :cl
        :rove))
(in-package :aoc/2019/day8)

;;; Part 1

(defun parse-image-str (str width height)
  (setf str (string-trim '(#\Space #\Newline) str))
  (let ((layers ()))
    (loop
      with layer = (make-array (list height width) :element-type 'integer)
      with x = 0
      with y = 0
      for c across str
      do (setf (aref layer y x) (digit-char-p c))
      (incf x)
      (when (= x width)
        (setf x 0)
        (incf y)
        (when (= y height)
          (push layer layers)
          (setf layer (make-array (list height width) :element-type 'integer))
          (setf y 0))))
    (nreverse layers)))

(defun find-part-1-answer (sif-image)
  (let ((min-0-digits nil)
        (min-0-digits-layer))
    (loop
      for layer in sif-image
      for zero-digits = 0
      do (loop
           for y below (array-dimension layer 0)
           do (loop
                for x below (array-dimension layer 1)
                for digit = (aref layer y x)
                if (= digit 0) do (incf zero-digits)))
      (when (or (not min-0-digits) (< zero-digits min-0-digits))
        (setf min-0-digits zero-digits)
        (setf min-0-digits-layer layer)))
    (loop
      with one-digits = 0
      with two-digits = 0
      for y below (array-dimension min-0-digits-layer 0)
      do (loop
           for x below (array-dimension min-0-digits-layer 1)
           for digit = (aref min-0-digits-layer y x)
           if (= digit 1) do (incf one-digits)
           else if (= digit 2) do (incf two-digits))
      finally (return (* one-digits two-digits)))))

(deftest part-1-test
  (testing "parse-image-str"
    (ok (equalp (parse-image-str "123456789012" 3 2) '(#2A((1 2 3) (4 5 6)) #2A((7 8 9) (0 1 2)))))))

;;; Part 2

(defun decode-image (sif-image)
  (let* ((height (array-dimension (first sif-image) 0))
         (width (array-dimension (first sif-image) 1))
         (out-image (make-array (list height width) :element-type 'integer)))
    (loop
      for y below height
      do (loop
           for x below width
           do (loop
                for layer in sif-image
                for digit = (aref layer y x)
                do (ecase digit
                     (0 (setf (aref out-image y x) 0) (return))
                     (1 (setf (aref out-image y x) 1) (return))
                     (2)))))
    out-image))

(deftest part-2-test
  (testing "decode-image"
    (ok (equalp (decode-image (parse-image-str "0222112222120000" 2 2)) #2A((0 1) (1 0))))))
