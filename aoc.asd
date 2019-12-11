(defsystem "aoc"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("uiop" "genhash" "rove")
  :components ((:module "src/2018"
                :components ((:file "day1")
                             (:file "day2")))
               (:module "src/2019"
                :components ((:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5"))))
  :description ""
  :perform (test-op (op c) (symbol-call :rove :run c)))
