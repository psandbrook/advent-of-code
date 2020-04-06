(defpackage aoc/2019/day7
  (:use :cl
        :uiop
        :bordeaux-threads
        :rove)
  (:import-from :chanl :unbounded-channel :send :recv))
(in-package :aoc/2019/day7)

;;; Parts 1 & 2

(defparameter *global-standard-output* *standard-output*)
(defparameter *global-standard-output-lock* (make-lock))

(defun atomic-stdout-format (&rest format-args)
  (with-lock-held (*global-standard-output-lock*)
    (apply #'format *global-standard-output* format-args)))

(defun run-intcode (mem
                     &key
                     (input-chanl *standard-input*)
                     (output-chanl *standard-output*))
  (macrolet ((deref (pos) `(elt mem (elt mem ,pos))))
    (flet ((read-input ()
             (etypecase input-chanl
               (stream (read input-chanl))
               (unbounded-channel (recv input-chanl))))
           (write-output (output)
             (etypecase output-chanl
               (stream (print output output-chanl))
               (unbounded-channel (send output-chanl output)))))
      (loop
        with ip = 0
        for ip-value-str = (write-to-string (elt mem ip))
        for opcode-start = (max (- (length ip-value-str) 2) 0)
        for opcode = (parse-integer ip-value-str :start opcode-start)
        for param-modes = (nreverse (map 'list #'digit-char-p (subseq ip-value-str 0 opcode-start)))
        do (flet ((get-param (index)
                    (let ((pos (+ ip 1 index))
                          (mode (if (>= index (length param-modes)) 0 (nth index param-modes))))
                      (ecase mode
                        (0 (deref pos))
                        (1 (elt mem pos))))))
             (ecase opcode
               (1 (let ((p0 (get-param 0))
                        (p1 (get-param 1)))
                    (setf (deref (+ ip 3)) (+ p0 p1))
                    (incf ip 4)))
               (2 (let ((p0 (get-param 0))
                        (p1 (get-param 1)))
                    (setf (deref (+ ip 3)) (* p0 p1))
                    (incf ip 4)))
               (3 (let ((input (read-input)))
                    (setf (deref (+ ip 1)) input)
                    (incf ip 2)))
               (4 (let ((p0 (get-param 0)))
                    (write-output p0)
                    (incf ip 2)))
               (5 (let ((p0 (get-param 0))
                        (p1 (get-param 1)))
                    (if (/= p0 0) (setf ip p1) (incf ip 3))))
               (6 (let ((p0 (get-param 0))
                        (p1 (get-param 1)))
                    (if (= p0 0) (setf ip p1) (incf ip 3))))
               (7 (let ((p0 (get-param 0))
                        (p1 (get-param 1)))
                    (setf (deref (+ ip 3)) (if (< p0 p1) 1 0))
                    (incf ip 4)))
               (8 (let ((p0 (get-param 0))
                        (p1 (get-param 1)))
                    (setf (deref (+ ip 3)) (if (= p0 p1) 1 0))
                    (incf ip 4)))
               (99 (return))))))))

(defun parse-intcode-str (str)
  (map 'vector #'parse-integer (split-string str :separator ",")))

(defun find-highest-signal (input-mem)
  (let ((highest-signal -1))
    (flet ((run-amplifier (phase-setting input-signal)
             (with-input-from-string (in (format nil "~s ~s" phase-setting input-signal))
               (parse-integer (with-output-to-string (out)
                                (run-intcode (copy-seq input-mem)
                                             :input-chanl in
                                             :output-chanl out))))))
      (dotimes (phase1 5)
        (dotimes (phase2 5)
          (dotimes (phase3 5)
            (dotimes (phase4 5)
              (dotimes (phase5 5)
                (if (/= phase1 phase2 phase3 phase4 phase5)
                    (let ((final-signal (run-amplifier
                                          phase5
                                          (run-amplifier
                                            phase4
                                            (run-amplifier
                                              phase3
                                              (run-amplifier
                                                phase2
                                                (run-amplifier phase1 0)))))))
                      (setf highest-signal (max highest-signal final-signal))))))))))
    highest-signal))

(deftest part-1-test
  (testing "run-intcode"
           (flet ((check-intcode-prog (str out)
                    (let ((mem (parse-intcode-str str)))
                      (run-intcode mem)
                      (equalp mem (parse-intcode-str out)))))
             (ok (check-intcode-prog "1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"))
             (ok (check-intcode-prog "1,0,0,0,99" "2,0,0,0,99"))
             (ok (check-intcode-prog "2,3,0,3,99" "2,3,0,6,99"))
             (ok (check-intcode-prog "2,4,4,5,99,0" "2,4,4,5,99,9801"))
             (ok (check-intcode-prog "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"))))
  (testing "find-highest-signal"
           (ok (= (find-highest-signal (parse-intcode-str "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
                  43210))
           (ok (= (find-highest-signal (parse-intcode-str "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
                  54321))
           (ok (= (find-highest-signal (parse-intcode-str "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
                  65210))))

(defun last-elt (l) (car (last l)))

(defun find-highest-feedback-signal (input-mem)
  (let ((highest-signal -1))
    (flet ((exec-feedback-loop (phase-settings)
             (flet ((make-amp (input-chanl output-chanl &key name)
                      (list (make-thread #'(lambda () (run-intcode (copy-seq input-mem)
                                                                   :input-chanl input-chanl
                                                                   :output-chanl output-chanl))
                                         :name name)
                            input-chanl
                            output-chanl)))
               (let ((amps ()))
                 (loop
                   with input-chanl = (make-instance 'unbounded-channel)
                   with output-chanl = (make-instance 'unbounded-channel)
                   for i below 4
                   do (send input-chanl (nth i phase-settings))
                   (push (make-amp input-chanl output-chanl :name (format nil "Amp ~a" (1+ i))) amps)
                   (setf input-chanl output-chanl)
                   (setf output-chanl (make-instance 'unbounded-channel)))
                 (setf amps (nreverse amps))
                 (let ((last-input-chanl (third (last-elt amps))))
                   (send last-input-chanl (nth 4 phase-settings))
                   (push (make-amp last-input-chanl (second (first amps)) :name "Amp 5") amps)
                   (send (second (first amps)) 0)
                   (join-thread (first (last-elt amps)))
                   (recv (third (last-elt amps))))))))
      (loop
        for phase1 from 5 to 9
        do (loop
             for phase2 from 5 to 9
             do (loop
                  for phase3 from 5 to 9
                  do (loop
                       for phase4 from 5 to 9
                       do (loop
                            for phase5 from 5 to 9
                            if (/= phase1 phase2 phase3 phase4 phase5)
                            do (let ((final-signal (exec-feedback-loop (list phase1 phase2 phase3 phase4 phase5))))
                                 (setf highest-signal (max highest-signal final-signal))))))))
      highest-signal)))

(deftest part-2-test
  (testing "find-highest-feedback-signal"
           (ok (= (find-highest-feedback-signal (parse-intcode-str "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
                  139629729))
           (ok (= (find-highest-feedback-signal (parse-intcode-str "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))
                  18216))))
