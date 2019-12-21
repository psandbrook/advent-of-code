(defsystem :aoc
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:3d-vectors :alexandria :bordeaux-threads :chanl :iterate :uiop :genhash :rove)
  :components ((:file "src/util")
               (:module "src/2018"
                        :components ((:file "day1")
                                     (:file "day2")))
               (:module "src/2019"
                        :depends-on ("src/util")
                        :components ((:file "day1")
                                     (:file "day2")
                                     (:file "day3")
                                     (:file "day4")
                                     (:file "day5")
                                     (:file "day6")
                                     (:file "day7")
                                     (:file "day8")
                                     (:file "day9")
                                     (:file "day10")
                                     (:file "day11")
                                     (:file "day12"))))
  :description ""
  :perform (test-op (op c) (symbol-call :rove :run c)))
