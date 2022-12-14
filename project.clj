(defproject aoc2015 "0.1.0-SNAPSHOT"
  :description "Advent Of Code 2015"
  ; :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.json "2.4.0"]
                 [net.mikera/core.matrix "0.62.0"]]
  ; :main ^:skip-aot aoc2015.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
