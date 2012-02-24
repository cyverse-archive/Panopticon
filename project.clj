(defproject panopticon "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.json "0.1.1"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.iplantc/clojure-commons "1.1.0-SNAPSHOT"]
                 [clj-time "0.3.7"]]
  :aot [panopticon.core]
  :main panopticon.core)