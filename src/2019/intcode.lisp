(defpackage aoc/2019/intcode
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util)
  (:export :intcode-computer
           :mem
           :parse-intcode-str
           :make-intcode-computer
           :run-computer-1
           :run-computer-for-input
           :run-computer-for-output
           :run-computer))
(in-package :aoc/2019/intcode)

(defclass intcode-computer ()
  ((mem :initarg :mem :accessor mem)
   (ip :initform 0 :accessor ip)
   (relative-base :initform 0 :accessor relative-base)))

(defun parse-intcode-str (str)
  (mapcar #'parse-integer (str:split #\, str)))

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

(defun run-computer-1 (comp &key
                            (input-f (lambda () (format t "INPUT: ") (read)))
                            (output-f (lambda (x) (format t "OUTPUT: ~a~%" x))))
  (with-accessors ((ip ip) (relative-base relative-base)) comp
    (let* ((ip-value-str (write-to-string (get-pos comp ip)))
           (opcode-start (max (- (length ip-value-str) 2) 0))
           (opcode (parse-integer ip-value-str :start opcode-start))
           (param-modes (nreverse (map 'list #'digit-char-p (subseq ip-value-str 0 opcode-start)))))
      (flet ((get-param (index)
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
               (incf ip 2)
               (return-from run-computer-1 'input)))
          (4 (let ((p0 (get-param 0)))
               (funcall output-f p0)
               (incf ip 2)
               (return-from run-computer-1 'output)))
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
          (99 (return-from run-computer-1 'stop)))
        'continue))))

(defun run-computer-for-input (comp input)
  (iter
    (case (run-computer-1 comp :input-f (constantly input))
      (input (leave t))
      (stop (leave nil)))))

(defun run-computer-for-output (comp)
  (let (result)
    (iter
      (case (run-computer-1 comp :output-f (lambda (x) (setf result x)))
        (output (leave result))
        (stop (leave nil))))))

(defun run-computer (comp &key input-f output-f)
  (let ((args ()))
    (if input-f (nconcf args (list :input-f input-f)))
    (if output-f (nconcf args (list :output-f output-f)))
    (iter
      (until (eq (apply #'run-computer-1 comp args) 'stop)))))
