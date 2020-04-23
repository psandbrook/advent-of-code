(defsystem :aoc
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:3d-vectors
               :alexandria
               :iterate
               :lparallel
               :str
               :uiop
               :genhash
               :rove)
  :components ((:file "src/util")
               (:module "src/2018"
                :components ((:file "day1")
                             (:file "day2")))
               (:module "src/2019"
                :depends-on ("src/util")
                :components ((:file "intcode")
                             (:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")
                             (:file "day7" :depends-on ("intcode"))
                             (:file "day8")
                             (:file "day9")
                             (:file "day10")
                             (:file "day11" :depends-on ("intcode"))
                             (:file "day12")
                             (:file "day13" :depends-on ("intcode"))
                             (:file "day14")
                             (:file "day15" :depends-on ("intcode"))
                             (:file "day16")
                             (:file "day17" :depends-on ("intcode"))
                             (:file "day18")
                             (:file "day19" :depends-on ("intcode"))
                             (:file "day20")
                             (:file "day21" :depends-on ("intcode"))
                             (:file "day22")
                             (:file "day23" :depends-on ("intcode"))
                             (:file "day24"))))
  :description ""
  :perform (test-op (op c) (symbol-call :rove :run c)))
