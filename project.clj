(defproject com.gfredericks/chess "0.0.5"
  :description "Okay I am writing this chess thing."
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[criterium "0.4.2"]
                                  [reiddraper/simple-check "0.5.2"]]
                   :jvm-opts ^:replace []}})
