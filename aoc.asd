(defsystem "aoc"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("3d-vectors" "alexandria" "bordeaux-threads" "chanl" "uiop" "genhash" "rove")
  :components ((:module "src/2018"
                :components ((:file "day1")
                             (:file "day2")))
               (:module "src/2019"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7")
                             (:file "day8")
                             (:file "day9")
                             (:file "day10"))))
  :description ""
  :perform (test-op (op c) (symbol-call :rove :run c)))
