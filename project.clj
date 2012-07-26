(defproject panopticon "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.1"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.iplantc/clojure-commons "1.2.0-SNAPSHOT"]
                 [clj-time "0.3.7"]
                 [slingshot "0.10.1"]]
  :plugins [[org.iplantc/lein-iplant-rpm "1.3.1-SNAPSHOT"]]
  :iplant-rpm {:summary "panopticon"
               :runuser "condor"
               :dependencies ["iplant-service-config >= 0.1.0-5"]
               :config-files ["log4j.properties"]
               :config-path "conf"}
  :aot [panopticon.core]
  :main panopticon.core
  :repositories {"iplantCollaborative"
                 "http://projects.iplantcollaborative.org/archiva/repository/internal/"})
