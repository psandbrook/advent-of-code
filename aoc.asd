(defsystem "aoc"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("rove")
  :components ((:module "src"
                :components
                ((:file "day1")
                 (:file "day2"))))
  :description ""
  :perform (test-op (op c) (symbol-call :rove :run c)))
