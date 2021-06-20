(defproject com.gfredericks/chess "0.0.7"
  :description "Okay I am writing this chess thing."
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/test.check "1.0.0"]]
  :profiles {:dev {:dependencies []}
             :profiling {:dependencies [[criterium "0.4.2"]]
                         :jvm-opts ^:replace []}})
