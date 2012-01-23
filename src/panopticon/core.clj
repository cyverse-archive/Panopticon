(ns panopticon.core
  (:gen-class)
  (:require [clojure-commons.osm :as osm]
            [clojure-commons.props :as props]
            [clojure-commons.clavin-client :as cl]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]))

(def props (atom nil))

(def SUBMITTED "Submitted")
(def UNEXPANDED "Unexpanded")
(def IDLE "Idle")
(def RUNNING "Running")
(def REMOVED "Removed")
(def COMPLETED "Completed")
(def HELD "Held")
(def SUBERR "Submission_err")
(def FAILED "Failed")

(def JOBSTATUS
  {"0" UNEXPANDED
   "1" IDLE
   "2" RUNNING
   "3" REMOVED
   "4" COMPLETED
   "5" HELD
   "6" SUBERR})

(defn osm-url [] (get @props "panopticon.osm.url"))
(defn osm-coll [] (get @props "panopticon.osm.collection"))
(defn condor-config [] (get @props "panopticon.condor.condor-config"))
(defn condor-q [] (get @props "panopticon.condor.condor-q"))
(defn condor-history [] (get @props "panopticon.condor.condor-history"))
(defn osm-client [] (osm/create osm-url osm-coll))

(defn running-jobs 
  [osm-client]
  (let [query {"$or" [{"state.status" RUNNING} {"state.status" SUBMITTED}]}]
    (:objects (json/read-json (osm/query osm-client query)))))

(defn history
  [uuid]
  (let [ipc-uuid (str "IpcUuid ==\"" uuid "\"")
        cmd      ["condor_history" "-l" "-constraint" ipc-uuid]]
    (apply sh/sh cmd)))

(defn queue
  [uuid]
  (let [ipc-uuid (str "IpcUuid ==\"" uuid "\"")
        cmd      ["condor_q" "-list" "-constraint" ipc-uuid]]
    (apply sh/sh cmd)))

(defn -main
  [& args]
  (def zkprops (props/parse-properties "panopticon.properties"))
  (def zkurl (get zkprops "zookeeper"))
  
  (cl/with-zk
    zkurl
    (when (not (cl/can-run?))
      (log/warn "THIS APPLICATION CANNOT RUN ON THIS MACHINE. SO SAYETH ZOOKEEPER.")
      (log/warn "THIS APPLICATION WILL NOT EXECUTE CORRECTLY.")
      (System/exit 1))
    
    (reset! props (cl/properties "panopticon")))
  
  (loop []
    ;;Grab running or submitted jobs from the OSM.
    ;;Look up jobs in in condor_q and condor_history.
    ;;Update the job maps.
    ;;Push the updated job maps into the OSM.
    (recur)))
