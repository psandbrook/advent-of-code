(defpackage aoc/2020/day8
  (:use :cl :alexandria :iter :rove :aoc/util))
(in-package :aoc/2020/day8)

(defun read-data-file-lines (name)
  (uiop:read-file-lines (merge-pathnames name "data/2020/day8/")))

(defun parse-code (lines)
  (iter (for line in lines)
    (collect (destructuring-bind (op-s arg-s) (str:words line)
               (list (str:string-case op-s
                       ("acc" 'acc)
                       ("jmp" 'jmp)
                       ("nop" 'nop))
                     (parse-integer arg-s)))
      result-type vector)))

(defun find-acc-before-loop (code)
  (iter
    (with pc = 0)
    (with acc = 0)
    (with run-before = (make-array (length code) :initial-element nil))
    (if (elt run-before pc) (leave acc))
    (setf (elt run-before pc) t)
    (destructuring-bind (op arg) (elt code pc)
      (ccase op
        (acc
         (incf acc arg)
         (incf pc))
        (jmp (incf pc arg))
        (nop (incf pc))))))

(defun find-acc-before-loop-file (input-name)
  (find-acc-before-loop (parse-code (read-data-file-lines input-name))))

(deftest find-acc-before-loop-file
  (ok (= (find-acc-before-loop-file "test-input-1.txt") 5))
  (ok (= (find-acc-before-loop-file "input.txt") 1939)))

(defun find-acc-fixed (code)
  (iter (for i index-of-vector code)
    (for (op arg) = (elt code i))
    (case op
      ((jmp nop)
       (let ((fixed-code (copy-seq code)))
         (setf (elt fixed-code i) (list (ecase op (jmp 'nop) (nop 'jmp)) arg))
         (iter
           (with pc = 0)
           (with acc = 0)
           (with run-before = (make-array (length fixed-code) :initial-element nil))
           (if (= pc (length fixed-code)) (return-from find-acc-fixed acc))
           (if (elt run-before pc) (finish))
           (setf (elt run-before pc) t)
           (for (op arg) = (elt fixed-code pc))
           (ccase op
             (acc
              (incf acc arg)
              (incf pc))
             (jmp (incf pc arg))
             (nop (incf pc)))))))))

(defun find-acc-fixed-file (input-name)
  (find-acc-fixed (parse-code (read-data-file-lines input-name))))

(deftest find-acc-fixed-file
  (ok (= (find-acc-fixed-file "test-input-1.txt") 8))
  (ok (= (find-acc-fixed-file "input.txt") 2212)))
