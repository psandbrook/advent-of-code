(defpackage aoc/2019/day14
  (:use :cl
        :alexandria
        :iterate
        :rove
        :aoc/util)
  (:import-from :str :lines :split :concat))
(in-package :aoc/2019/day14)

;;; Part 1

(defun parse-reactions-str (str)
  (flet ((parse-quantity (str)
           (destructuring-bind (n name) (split #\Space str :omit-nulls t)
             (list (parse-integer n) name))))
    (iter (for line in (lines str))
          (for (inputs output) = (split "=>" line))
          (collect (list (mapcar #'parse-quantity (split #\, inputs)) (parse-quantity output))))))

(defun required-ore (reactions)
  (let ((quantities (make-hash-table :test #'equalp)))
    (setf (gethash "FUEL" quantities) 1)
    (iter (with allow-leftovers = nil)
          (for new-quantities = (make-hash-table :test #'equalp))
          (iter (for (name n) in-hashtable quantities)
                (when (equal name "ORE")
                  (ensure-gethash name new-quantities 0)
                  (incf (gethash name new-quantities) n)
                  (next-iteration))

                (for (inputs output) = (find name reactions :key (lambda (q) (second (second q))) :test #'equal))
                (for times = (if allow-leftovers (ceiling n (first output)) (/ n (first output))))
                (if (>= times 1) (setf allow-leftovers nil))
                (iter (while (>= times 1))
                      (iter (for (in-n in-name) in inputs)
                            (ensure-gethash in-name new-quantities 0)
                            (incf (gethash in-name new-quantities) in-n))
                      (decf times 1)
                      (decf n (first output)))
                (when (/= n 0)
                  (ensure-gethash name new-quantities 0)
                  (incf (gethash name new-quantities) n)))

          (setf allow-leftovers (hash-equal quantities new-quantities :test #'equalp))
          (setf quantities new-quantities)
          (maphash (lambda (name n)
                     (if (and (not (equal name "ORE"))
                              (> n 0))
                         (next-iteration)))
                   quantities)
          (finish))
    (values (gethash "ORE" quantities) quantities)))

(deftest part-1-test
  (testing "required-ore"
           (ok (= (required-ore (parse-reactions-str (concat "10 ORE => 10 A" *newline-str*
                                                             "1 ORE => 1 B" *newline-str*
                                                             "7 A, 1 B => 1 C" *newline-str*
                                                             "7 A, 1 C => 1 D" *newline-str*
                                                             "7 A, 1 D => 1 E" *newline-str*
                                                             "7 A, 1 E => 1 FUEL")))
                  31))
           (ok (= (required-ore (parse-reactions-str (concat "9 ORE => 2 A" *newline-str*
                                                             "8 ORE => 3 B" *newline-str*
                                                             "7 ORE => 5 C" *newline-str*
                                                             "3 A, 4 B => 1 AB" *newline-str*
                                                             "5 B, 7 C => 1 BC" *newline-str*
                                                             "4 C, 1 A => 1 CA" *newline-str*
                                                             "2 AB, 3 BC, 4 CA => 1 FUEL")))
                  165))
           (ok (= (required-ore (parse-reactions-str (concat "157 ORE => 5 NZVS" *newline-str*
                                                             "165 ORE => 6 DCFZ" *newline-str*
                                                             "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL" *newline-str*
                                                             "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ" *newline-str*
                                                             "179 ORE => 7 PSHF" *newline-str*
                                                             "177 ORE => 5 HKGWZ" *newline-str*
                                                             "7 DCFZ, 7 PSHF => 2 XJWVT" *newline-str*
                                                             "165 ORE => 2 GPVTF" *newline-str*
                                                             "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")))
                  13312))
           (ok (= (required-ore (parse-reactions-str (concat "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG" *newline-str*
                                                             "17 NVRVD, 3 JNWZP => 8 VPVL" *newline-str*
                                                             "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL" *newline-str*
                                                             "22 VJHF, 37 MNCFX => 5 FWMGM" *newline-str*
                                                             "139 ORE => 4 NVRVD" *newline-str*
                                                             "144 ORE => 7 JNWZP" *newline-str*
                                                             "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC" *newline-str*
                                                             "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV" *newline-str*
                                                             "145 ORE => 6 MNCFX" *newline-str*
                                                             "1 NVRVD => 8 CXFTF" *newline-str*
                                                             "1 VJHF, 6 MNCFX => 4 RFSQX" *newline-str*
                                                             "176 ORE => 6 VJHF")))
                  180697))
           (ok (= (required-ore (parse-reactions-str (concat "171 ORE => 8 CNZTR" *newline-str*
                                                             "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL" *newline-str*
                                                             "114 ORE => 4 BHXH" *newline-str*
                                                             "14 VRPVC => 6 BMBT" *newline-str*
                                                             "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL" *newline-str*
                                                             "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT" *newline-str*
                                                             "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW" *newline-str*
                                                             "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW" *newline-str*
                                                             "5 BMBT => 4 WPTQ" *newline-str*
                                                             "189 ORE => 9 KTJDG" *newline-str*
                                                             "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP" *newline-str*
                                                             "12 VRPVC, 27 CNZTR => 2 XDBXC" *newline-str*
                                                             "15 KTJDG, 12 BHXH => 5 XCVML" *newline-str*
                                                             "3 BHXH, 2 VRPVC => 7 MZWV" *newline-str*
                                                             "121 ORE => 7 VRPVC" *newline-str*
                                                             "7 XCVML => 6 RJRHP" *newline-str*
                                                             "5 BHXH, 4 VRPVC => 5 LTCX")))
                  2210736))))

;;; Part 2

(defparameter *ore-n* 1000000000000)

(defun max-fuel (reactions)
  (let ((quantities (nth-value 1 (required-ore reactions)))
        (acc-quantities (make-hash-table :test #'equalp))
        (fuel 0))

    (flet ((add-quantities ()
             (let ((times (max (floor (- *ore-n* (gethash "ORE" acc-quantities 0)) (gethash "ORE" quantities))
                               1)))

               (iter (for (name n) in-hashtable quantities)
                     (ensure-gethash name acc-quantities 0)
                     (incf (gethash name acc-quantities) (* times n)))
               (incf fuel times)))

           (reduce-quantities ()
             (iter (with cont = nil)
                   (setf cont nil)
                   (iter (for (name n) in-hashtable acc-quantities)
                         (if (or (equal name "ORE") (>= n 0)) (next-iteration))

                         (for (inputs output) = (find name reactions
                                                      :key (lambda (q) (second (second q)))
                                                      :test #'equal))
                         (for times = (floor (abs n) (first output)))
                         (when (> times 0)
                           (setf cont t)
                           (iter (for (in-n in-name) in inputs)
                                 (ensure-gethash in-name acc-quantities 0)
                                 (decf (gethash in-name acc-quantities) (* times in-n)))
                           (incf (gethash name acc-quantities) (* times (first output)))))
                   (while cont))))

      (iter (for prev-fuel = fuel)
            (add-quantities)
            (reduce-quantities)
            (if (> (gethash "ORE" acc-quantities) *ore-n*) (leave prev-fuel))))))

(deftest part-2-test
  (testing "max-fuel"
           (ok (= (max-fuel (parse-reactions-str (concat "157 ORE => 5 NZVS" *newline-str*
                                                         "165 ORE => 6 DCFZ" *newline-str*
                                                         "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL" *newline-str*
                                                         "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ" *newline-str*
                                                         "179 ORE => 7 PSHF" *newline-str*
                                                         "177 ORE => 5 HKGWZ" *newline-str*
                                                         "7 DCFZ, 7 PSHF => 2 XJWVT" *newline-str*
                                                         "165 ORE => 2 GPVTF" *newline-str*
                                                         "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")))
                  82892753))
           (ok (= (max-fuel (parse-reactions-str (concat "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG" *newline-str*
                                                         "17 NVRVD, 3 JNWZP => 8 VPVL" *newline-str*
                                                         "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL" *newline-str*
                                                         "22 VJHF, 37 MNCFX => 5 FWMGM" *newline-str*
                                                         "139 ORE => 4 NVRVD" *newline-str*
                                                         "144 ORE => 7 JNWZP" *newline-str*
                                                         "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC" *newline-str*
                                                         "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV" *newline-str*
                                                         "145 ORE => 6 MNCFX" *newline-str*
                                                         "1 NVRVD => 8 CXFTF" *newline-str*
                                                         "1 VJHF, 6 MNCFX => 4 RFSQX" *newline-str*
                                                         "176 ORE => 6 VJHF")))
                  5586022))
           (ok (= (max-fuel (parse-reactions-str (concat "171 ORE => 8 CNZTR" *newline-str*
                                                         "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL" *newline-str*
                                                         "114 ORE => 4 BHXH" *newline-str*
                                                         "14 VRPVC => 6 BMBT" *newline-str*
                                                         "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL" *newline-str*
                                                         "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT" *newline-str*
                                                         "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW" *newline-str*
                                                         "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW" *newline-str*
                                                         "5 BMBT => 4 WPTQ" *newline-str*
                                                         "189 ORE => 9 KTJDG" *newline-str*
                                                         "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP" *newline-str*
                                                         "12 VRPVC, 27 CNZTR => 2 XDBXC" *newline-str*
                                                         "15 KTJDG, 12 BHXH => 5 XCVML" *newline-str*
                                                         "3 BHXH, 2 VRPVC => 7 MZWV" *newline-str*
                                                         "121 ORE => 7 VRPVC" *newline-str*
                                                         "7 XCVML => 6 RJRHP" *newline-str*
                                                         "5 BHXH, 4 VRPVC => 5 LTCX")))
                  460664))))
