(defpackage aoc/2020/day4
  (:use :cl :alexandria :iter :rove :aoc/util))
(in-package :aoc/2020/day4)

(defun read-data-file-string (name)
  (uiop:read-file-string (merge-pathnames name "data/2020/day4/")))

(defun parse-passports (string)
  (let ((output nil)
        (passport-strings (str:split (coerce '(#\Newline #\Newline) 'string) string)))
    (iter (for s in passport-strings)
      (let ((passport nil))
        (iter (for field in (ppcre:split "\\s" s))
          (destructuring-bind (name value) (str:split #\: field)
            (push (cons name value) passport)))
        (push passport output)))
    output))

(defun passport-valid-p (passport)
  (iter (for field-name in '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
    (always (assoc field-name passport :test #'equal))))

(defun count-valid-passports (passports)
  (iter (for passport in passports)
    (counting (passport-valid-p passport))))

(defun count-valid-passports-file (name)
  (count-valid-passports (parse-passports (read-data-file-string name))))

(deftest count-valid-passports-file
  (ok (= (count-valid-passports-file "test-input-1.txt") 2))
  (ok (= (count-valid-passports-file "input.txt") 213)))

(defun passport-valid-p-2 (passport)
  (and (passport-valid-p passport)
       (<= 1920 (parse-integer (assoc-value passport "byr" :test #'equal)) 2002)
       (<= 2010 (parse-integer (assoc-value passport "iyr" :test #'equal)) 2020)
       (<= 2020 (parse-integer (assoc-value passport "eyr" :test #'equal)) 2030)
       (let ((s (assoc-value passport "hgt" :test #'equal)))
         (multiple-value-bind (integer pos) (parse-integer s :junk-allowed t)
           (let ((units (subseq s pos)))
             (cond ((equal units "cm") (<= 150 integer 193))
                   ((equal units "in") (<= 59 integer 76))
                   (t nil)))))
       (let ((s (assoc-value passport "hcl" :test #'equal)))
         (and (= (length s) 7)
              (eql (elt s 0) #\#)
              (iter (for c in-string (subseq s 1))
                (always (or (char<= #\0 c #\9) (char<= #\a c #\f))))))
       (find (assoc-value passport "ecl" :test #'equal) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'equal)
       (let ((s (assoc-value passport "pid" :test #'equal)))
         (and (= (length s) 9)
              (iter (for c in-string s)
                (always (char<= #\0 c #\9)))))))

(defun count-valid-passports-2 (passports)
  (iter (for passport in passports)
    (counting (passport-valid-p-2 passport))))

(defun count-valid-passports-file-2 (name)
  (count-valid-passports-2 (parse-passports (read-data-file-string name))))

(deftest count-valid-passports-file-2
  (ok (= (count-valid-passports-file-2 "test-input-2.txt") 4))
  (ok (= (count-valid-passports-file-2 "input.txt") 147)))
