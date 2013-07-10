(defproject panopticon "1.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.iplantc/clojure-commons "1.4.0-SNAPSHOT"]
                 [cheshire "5.0.1"]
                 [clj-time "0.4.4"]
                 [slingshot "0.10.3"]]
  :plugins [[org.iplantc/lein-iplant-rpm "1.4.1-SNAPSHOT"]]
  :iplant-rpm {:summary "panopticon"
               :runuser "condor"
               :dependencies ["iplant-service-config >= 0.1.0-5" "iplant-clavin"]
               :config-files ["log4j.properties"]
               :config-path "conf"}
  :aot [panopticon.core]
  :main panopticon.core
  :repositories {"iplantCollaborative"
                 "http://projects.iplantcollaborative.org/archiva/repository/internal/"})
