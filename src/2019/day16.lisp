(defpackage aoc/2019/day16
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util)
  (:import-from :str :concat :trim))
(in-package :aoc/2019/day16)

;;; Parts 1 & 2

(defun parse-fft-input (str)
  (map 'vector #'digit-char-p (trim str)))

(defun fft-phase (input)
  (iter (for out-i below (length input))
        (for x = (iter (with section = 0)
                       (with i-in-section = 0)
                       (for digit in-vector input)
                       (incf i-in-section)
                       (when (> i-in-section out-i)
                         (setf i-in-section 0)
                         (incf section)
                         (if (> section 3) (setf section 0)))
                       (ecase section
                         ((0 2))
                         (1 (sum digit))
                         (3 (sum (* -1 digit))))))
        (collect (nth-value 1 (truncate (abs x) 10)) result-type 'vector)))

(defun fft-phase-2 (input)
  (iter (for digit in-vector input downto 0)
        (sum digit into acc-sum)
        (collect (nth-value 1 (truncate (abs acc-sum) 10)) at beginning result-type 'vector)))

(defun fft (input n-phases &optional (phase-f #'fft-phase))
  (iter (with out-digits = input)
        (repeat n-phases)
        (setf out-digits (funcall phase-f out-digits))
        (finally (return out-digits))))

(defun make-real-signal (input)
  (let ((offset (parse-integer (map 'string #'digit-char (subseq input 0 7)))))
    (iter (with out = (make-array 0 :adjustable t :fill-pointer 0))
          (with i = 0)
          (repeat 10000)
          (iter (for digit in-vector input)
                (when (>= i offset) (vector-push-extend digit out))
                (incf i))
          (finally (assert (>= offset (/ i 2)))
                   (return out)))))

(deftest parts-1-2-test
  (testing "fft"
           (ok (equalp (fft (parse-fft-input "12345678") 4) (parse-fft-input "01029498")))

           (ok (equalp (subseq (fft (parse-fft-input "80871224585914546619083218645595") 100) 0 8)
                       (parse-fft-input "24176176")))

           (ok (equalp (subseq (fft (parse-fft-input "19617804207202209144916044189917") 100) 0 8)
                       (parse-fft-input "73745418")))

           (ok (equalp (subseq (fft (parse-fft-input "69317163492948606335995924319873") 100) 0 8)
                       (parse-fft-input "52432133"))))

  (testing "fft-2"
           (ok (equalp (subseq (fft (make-real-signal (parse-fft-input "03036732577212944063491565474664"))
                                    100 #'fft-phase-2)
                               0 8)
                       (parse-fft-input "84462026")))

           (ok (equalp (subseq (fft (make-real-signal (parse-fft-input "02935109699940807407585447034323"))
                                    100 #'fft-phase-2)
                               0 8)
                       (parse-fft-input "78725270")))

           (ok (equalp (subseq (fft (make-real-signal (parse-fft-input "03081770884921959731165446850517"))
                                    100 #'fft-phase-2)
                               0 8)
                       (parse-fft-input "53553731")))))
