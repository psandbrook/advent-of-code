(defpackage aoc/2019/day7
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util))
(in-package :aoc/2019/day7)

;;; Part 1

(defun find-highest-signal (ints)
  (let ((highest-signal -1))
    (flet ((run-amplifier (phase-setting input-signal)
             (let ((in (list phase-setting input-signal))
                   out)
               (intcode:run-computer (intcode:make-computer ints)
                                     :input-f (lambda () (pop in))
                                     :output-f (lambda (x) (setf out x)))
               out)))
      (dotimes (phase-1 5)
        (dotimes (phase-2 5)
          (dotimes (phase-3 5)
            (dotimes (phase-4 5)
              (dotimes (phase-5 5)
                (if (/= phase-1 phase-2 phase-3 phase-4 phase-5)
                    (let* ((sig-1 (run-amplifier phase-1 0))
                           (sig-2 (run-amplifier phase-2 sig-1))
                           (sig-3 (run-amplifier phase-3 sig-2))
                           (sig-4 (run-amplifier phase-4 sig-3))
                           (sig-5 (run-amplifier phase-5 sig-4)))
                      (setf highest-signal (max highest-signal sig-5))))))))))
    highest-signal))

(deftest part-1-test
  (testing "find-highest-signal"
           (ok (= (find-highest-signal (intcode:parse-intcode-str "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
                  43210))
           (ok (= (find-highest-signal (intcode:parse-intcode-str "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
                  54321))
           (ok (= (find-highest-signal (intcode:parse-intcode-str "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
                  65210))))

;;; Part 2

(defun find-highest-feedback-signal (ints)
  (let ((highest-signal -1))
    (iter (for phase-a from 5 to 9)
          (iter (for phase-b from 5 to 9)
                (iter (for phase-c from 5 to 9)
                      (iter (for phase-d from 5 to 9)
                            (iter (for phase-e from 5 to 9)
                                  (if (not (/= phase-a phase-b phase-c phase-d phase-e)) (next-iteration))

                                  (for amp-a = (intcode:make-computer ints))
                                  (for amp-b = (intcode:make-computer ints))
                                  (for amp-c = (intcode:make-computer ints))
                                  (for amp-d = (intcode:make-computer ints))
                                  (for amp-e = (intcode:make-computer ints))
                                  (intcode:run-computer-for-input amp-a phase-a)
                                  (intcode:run-computer-for-input amp-b phase-b)
                                  (intcode:run-computer-for-input amp-c phase-c)
                                  (intcode:run-computer-for-input amp-d phase-d)
                                  (intcode:run-computer-for-input amp-e phase-e)

                                  (intcode:run-computer-for-input amp-a 0)
                                  (iter
                                    (for sig-a = (intcode:run-computer-for-output amp-a))

                                    (intcode:run-computer-for-input amp-b sig-a)
                                    (for sig-b = (intcode:run-computer-for-output amp-b))

                                    (intcode:run-computer-for-input amp-c sig-b)
                                    (for sig-c = (intcode:run-computer-for-output amp-c))

                                    (intcode:run-computer-for-input amp-d sig-c)
                                    (for sig-d = (intcode:run-computer-for-output amp-d))

                                    (intcode:run-computer-for-input amp-e sig-d)
                                    (for sig-e = (intcode:run-computer-for-output amp-e))

                                    (when (not (intcode:run-computer-for-input amp-a sig-e))
                                      (setf highest-signal (max highest-signal sig-e))
                                      (finish))))))))
    highest-signal))

(deftest part-2-test
  (testing "find-highest-feedback-signal"
           (ok (= (find-highest-feedback-signal (intcode:parse-intcode-str "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
                  139629729))
           (ok (= (find-highest-feedback-signal (intcode:parse-intcode-str "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))
                  18216))))
