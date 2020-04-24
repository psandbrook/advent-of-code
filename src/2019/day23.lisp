(defpackage aoc/2019/day23
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util))
(in-package :aoc/2019/day23)

;;; Parts 1 & 2

(defun execute-network (ints &key nat)
  (let ((computers (make-array 50 :initial-element nil)))
    (iter (for i below (length computers))
          (setf (elt computers i) (list (intcode:make-computer ints) ()))
          (intcode:run-computer-for-input (first (elt computers i)) i))

    (iter
      (with i = 0)
      (with nat-packet)
      (with seen-nat-packets = (make-hash-table))
      (with no-send = 0)
      (with seen-send = nil)
      (iter
        (with receive-packet)
        (with send-packet)
        (case (intcode:run-computer-1 (first (elt computers i))
                                      :input-f (lambda ()
                                                 (if receive-packet
                                                     (pop receive-packet)
                                                     (if-let (p (rpop (second (elt computers i))))
                                                             (progn (setf receive-packet p)
                                                                    (pop receive-packet))
                                                             -1)))
                                      :output-f (lambda (x) (push x send-packet)))
          (intcode:input (if (not receive-packet) (finish)))
          (intcode:output (when (= (length send-packet) 3)
                            (nreversef send-packet)
                            (if (= (first send-packet) 255)
                                (if nat
                                    (setf nat-packet (rest send-packet))
                                    (return-from execute-network (third send-packet)))
                                (push (rest send-packet) (second (elt computers (first send-packet)))))
                            (setf send-packet ()
                                  seen-send t)
                            (finish)))))
      (incf i)
      (when (= i (length computers))
        (if seen-send
            (setf no-send 0)
            (incf no-send))
        (when (and nat
                   (>= no-send 3)
                   (iter (for (nil recv) in-vector computers) (always (emptyp recv))))
          (if-let (value (gethash (second nat-packet) seen-nat-packets))
                  (return-from execute-network (second nat-packet))
                  (setf (gethash (second nat-packet) seen-nat-packets) t))
          (push nat-packet (second (elt computers 0)))
          (setf no-send 0))
        (setf i 0
              seen-send nil)))))
