(defpackage aoc/2019/day4
  (:use :cl
        :rove))
(in-package :aoc/2019/day4)

;;; Part 1

(defun match-password (password)
  (let ((password-str (write-to-string password))
        (has-adjacent-digits nil)
        (digits-never-decrease t))
    (loop
      for i upto (- (length password-str) 2)
      for c1 = (elt password-str i)
      for c2 = (elt password-str (1+ i))
      for d1 = (digit-char-p c1)
      for d2 = (digit-char-p c2)
      if (eql c1 c2) do (setf has-adjacent-digits t)
      if (< d2 d1) do (setf digits-never-decrease nil))
    (and (= (length password-str) 6) has-adjacent-digits digits-never-decrease)))

(defun matches-in-range (start end)
  (loop for i from start to end if (match-password i) collect i))

(deftest part-1-test
  (testing "match-password"
    (ok (match-password 111111))
    (ng (match-password 223450))
    (ng (match-password 123789))))

;;; Part 2

(defun match-password-2 (password)
  (let ((password-str (write-to-string password))
        (has-adjacent-digits nil)
        (digits-never-decrease t))
    (loop named outer
      with i = 0
      with group-len
      for c = (elt password-str i)
      do (incf i)
         (setf group-len 1)
         (loop
           with c2
           if (>= i (length password-str))
             do (if (= group-len 2) (setf has-adjacent-digits t)) (return-from outer)
           do (setf c2 (elt password-str i))
           if (eql c c2) do (incf i) (incf group-len)
           else do (if (= group-len 2) (setf has-adjacent-digits t)) (return)))
    (loop
      for i upto (- (length password-str) 2)
      for d1 = (digit-char-p (elt password-str i))
      for d2 = (digit-char-p (elt password-str (1+ i)))
      if (< d2 d1) do (setf digits-never-decrease nil))
    (and (= (length password-str) 6) has-adjacent-digits digits-never-decrease)))

(defun matches-in-range-2 (start end)
  (loop for i from start to end if (match-password-2 i) collect i))

(deftest part-2-test
  (testing "match-password-2"
    (ok (match-password-2 112233))
    (ng (match-password-2 123444))
    (ok (match-password-2 111122))))
