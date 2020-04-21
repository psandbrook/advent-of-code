(defpackage aoc/2019/day22
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util))
(in-package :aoc/2019/day22)

;;; Part 1

(defun deal-into-new-stack (deck)
  (nreverse deck))

(defun cut (deck n)
  (if (< n 0)
      (let* ((n (- n))
             (n-cards (last deck n)))
        (setf (cdr (last deck (1+ n))) nil)
        (nconc n-cards deck))
      (let ((rest-cards (nthcdr n deck)))
        (setf (cdr (nthcdr (1- n) deck)) nil)
        (nconc rest-cards deck))))

(defun deal-with-increment (deck n)
  (let ((out (make-array (length deck) :initial-element nil)))
    (iter (for x in deck)
      (with index = 0)
      (setf (elt out index) x)
      (incf index n)
      (if (>= index (length out)) (decf index (length out))))
    (coerce out 'list)))

(defun parse-shuffle (str)
  (iter (for l in (str:lines str))
    (if (equal l "deal into new stack")
        (collect (list 'deal-into-new-stack))
        (multiple-value-bind (result suffix) (starts-with-subseq "deal with increment" l :return-suffix t)
          (if result
              (collect (list 'deal-with-increment (parse-integer suffix)))
              (multiple-value-bind (result suffix) (starts-with-subseq "cut" l :return-suffix t)
                (assert result)
                (collect (list 'cut (parse-integer suffix)))))))))

(defun execute-shuffle (shuffle deck)
  (iter (for (instr n) in shuffle)
    (setf deck (ecase instr
                 (deal-into-new-stack (deal-into-new-stack deck))
                 (cut (cut deck n))
                 (deal-with-increment (deal-with-increment deck n)))))
  deck)

(deftest part-1-test
  (testing "shuffle"
    (let ((deck (iter (for i below 10) (collect i))))
      (ok (equal (execute-shuffle (parse-shuffle (str:unlines (list "deal with increment 7"
                                                                    "deal into new stack"
                                                                    "deal into new stack")))
                                  (copy-list deck))
                 '(0 3 6 9 2 5 8 1 4 7)))

      (ok (equal (execute-shuffle (parse-shuffle (str:unlines (list "cut 6"
                                                                    "deal with increment 7"
                                                                    "deal into new stack")))
                                  (copy-list deck))
                 '(3 0 7 4 1 8 5 2 9 6)))

      (ok (equal (execute-shuffle (parse-shuffle (str:unlines (list "deal with increment 7"
                                                                    "deal with increment 9"
                                                                    "cut -2")))
                                  (copy-list deck))
                 '(6 3 0 7 4 1 8 5 2 9)))

      (ok (equal (execute-shuffle (parse-shuffle (str:unlines (list "deal into new stack"
                                                                    "cut -2"
                                                                    "deal with increment 7"
                                                                    "cut 8"
                                                                    "cut -4"
                                                                    "deal with increment 7"
                                                                    "cut 3"
                                                                    "deal with increment 9"
                                                                    "deal with increment 3"
                                                                    "cut -1")))
                                  (copy-list deck))
                 '(9 2 5 8 1 4 7 0 3 6))))))

;;; Part 2

(defun deal-into-new-stack-2 (deck)
  (destructuring-bind (offset increment) deck
    (let ((new-increment (- increment)))
      (setf (first deck) (+ offset new-increment)
            (second deck) new-increment)
      deck)))

(defun cut-2 (deck n)
  (destructuring-bind (offset increment) deck
    (setf (first deck) (+ offset (* increment n))
          (second deck) increment)
    deck))

(defun modular-expt (b e m)
  (if (= m 1)
      0
      (let ((result 1))
        (setf b (mod b m))
        (iter (while (> e 0))
          (when (= (mod e 2) 1)
            (setf result (mod (* result b) m)))
          (setf e (ash e -1)
                b (mod (* b b) m)))
        result)))

(defun deal-with-increment-2 (deck deck-length n)
  (destructuring-bind (offset increment) deck
    (declare (ignore offset))
    (setf (second deck) (* increment (modular-expt n (- deck-length 2) deck-length)))
    deck))

(defun execute-shuffle-2 (shuffle deck deck-length)
  (iter (for (instr n) in shuffle)
    (setf deck (ecase instr
                 (deal-into-new-stack (deal-into-new-stack-2 deck))
                 (cut (cut-2 deck n))
                 (deal-with-increment (deal-with-increment-2 deck deck-length n))))
    (iter (for cons on deck)
      (setf (car cons) (mod (car cons) deck-length))))
  deck)

(defun do-n-shuffles (shuffle deck-length n)
  (destructuring-bind (offset-diff increment-mul) (execute-shuffle-2 shuffle (list 0 1) deck-length)
    (let* ((new-increment (modular-expt increment-mul n deck-length))
           (result (list (* offset-diff
                            (1- new-increment)
                            (modular-expt (1- increment-mul) (- deck-length 2) deck-length))
                         new-increment)))
      (iter (for cons on result)
        (setf (car cons) (mod (car cons) deck-length)))
      result)))

(defun card-at (deck deck-length n)
  (destructuring-bind (offset increment) deck
    (mod (+ offset (* increment n)) deck-length)))
