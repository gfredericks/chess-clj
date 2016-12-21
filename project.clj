(defproject com.gfredericks/chess "0.0.7"
  :description "Okay I am writing this chess thing."
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}
             :profiling {:dependencies [[criterium "0.4.2"]]
                         :jvm-opts ^:replace []}})
