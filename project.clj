(defproject scheduljure "0.0.1-SNAPSHOT"
  :description "Generate schedule for one task, multiple people."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-time "0.13.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [spork "0.2.0.2-SNAPSHOT"]]
  :repl-options {:init-ns scheduljure.core})

