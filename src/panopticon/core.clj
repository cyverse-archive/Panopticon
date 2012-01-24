(ns panopticon.core
  (:gen-class)
  (:require [clojure-commons.osm :as osm]
            [clojure-commons.props :as props]
            [clojure-commons.clavin-client :as cl]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as string]))

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
(defn num-instances [] (Integer/parseInt (get @props "panopticon.app.num-instances")))

(def boolize #(boolean (Boolean. %)))
(defn any? [pred coll] ((comp not not-any?) pred coll))

(defn osm-client [] (osm/create (osm-url) (osm-coll)))
(defn job-status [classad-map] (get JOBSTATUS (get classad-map "JobStatus")))

(defn running-jobs 
  [osm-client]
  (let [query {"$or" [{"state.status" RUNNING} {"state.status" SUBMITTED}]}]
    (:objects (json/read-json (osm/query osm-client query)))))

(defn post-osm-updates
  [osm-objects osm-cl]
  (doseq [osm-object osm-objects]
    (let [new-state (:state osm-object)
          osm-id    (:object_persistence_uuid osm-object)]
      (log/debug "Posting object to the OSM: ")
      (log/debug new-state)
      (osm/update-object osm-cl osm-id new-state))))

(defn history
  [uuid]
  (let [ipc-uuid (str "IpcUuid ==\"" uuid "\"")
        cmd      ["condor_history" "-l" "-constraint" ipc-uuid]
        results  (apply sh/sh cmd)]
    (log/info cmd)
    (log/info (str "Exit Code: " (:exit results)))
    (log/warn (str "stderr: " (:err results)))
    (log/debug (str "stdout: " (:out results)))
    (:out results)))

(defn queue
  [uuid]
  (let [ipc-uuid (str "IpcUuid ==\"" uuid "\"")
        cmd      ["condor_q" "-long" "-constraint" ipc-uuid]
        results  (apply sh/sh cmd)]
    (log/info cmd)
    (log/info (str "Exit Code: " (:exit results)))
    (log/warn (str "stderr: " (:err results)))
    (log/debug (str "stdout: " (:out results)))
    (string/trim (:out results))))

(defn classad-lines
  [classad-str]
  (into [] 
        (filter 
          #(not (re-find #"^--" %)) 
          (-> classad-str (string/split #"\n")))))

(defn classad-maps
  [job-output]
  (into [] (for [classad-str (string/split job-output #"\n\n")]
             (apply merge 
                    (for [classad-line (classad-lines classad-str)]
                      (let [sections (string/split classad-line #"\=")
                            cl-key   (string/trim (first sections))
                            cl-val   (-> (string/join "=" (rest sections))
                                       (string/trim)
                                       (string/replace #"^\"" "")
                                       (string/replace #"\"$" ""))]
                        {cl-key cl-val}))))))

(defn parallel-func
  [func uuids]
  (let [anon-funcs (into [] (map #(partial (comp classad-maps func) %) uuids))]
    (into [] (flatten 
               (for [func-part (partition-all (num-instances) anon-funcs)]
                 (apply pcalls func-part))))))

(defn job-failed?
  [classad-map]
  (let [exit-by-signal (boolize (get classad-map "ExitBySignal"))
        exit-code      (get classad-map "ExitCode")
        job-status     (job-status classad-map)]
    (cond
      (= job-status REMOVED)    true
      (= job-status HELD)       true
      (= job-status SUBERR)     true
      (= job-status SUBMITTED)  false
      (= job-status UNEXPANDED) false
      (= job-status IDLE)       false
      (= job-status RUNNING)    false
      (and (= job-status COMPLETED) exit-by-signal)       true
      (and (= job-status COMPLETED) (not= exit-code "0")) true
      :else true)))

(defn analysis-submitted? [clds] (every? #(= (job-status %) SUBMITTED) clds))
(defn analysis-completed? [clds] (every? #(= (job-status %) COMPLETED) clds))
(defn analysis-running? [clds] (any? #(= (job-status %) RUNNING) clds))
(defn analysis-failed? [clds] (any? job-failed? clds))
(defn analysis-held? [clds] (any? #(= (job-status %) HELD) clds))

(defn analysis-status
  [clds]
  (cond
    (analysis-failed? clds)    FAILED
    (analysis-completed? clds) COMPLETED
    (analysis-submitted? clds) SUBMITTED
    (analysis-running? clds)   RUNNING
    (analysis-held? clds)      HELD
    :else                      IDLE))

(defn classads-for-osm-object
  [osm-object all-classads]
  (let [osm-uuid (:uuid (:state osm-object))]
    (into [] (filter #(= (get % "IpcUuid") osm-uuid) all-classads))))

(defn osm-job-for-classad
  [classad osm-object]
  (let [cl-job-id (keyword (get classad "IpcJobId"))
        osm-jobs (:jobs (:state osm-object))]
    [cl-job-id (get osm-jobs cl-job-id)]))

(defn update-jobs
  [osm-obj classads]
  (apply merge (for [classad classads]
                 (let [[job-id job] (osm-job-for-classad classad osm-obj)]
                   (assoc-in osm-obj [:state :jobs job-id :status] (job-status classad))))))

(defn update-osm-objects
  [osm-objects all-classads]
  (into [] (for [osm-obj osm-objects]
             (let [classads (classads-for-osm-object osm-obj all-classads)]
               (assoc-in (update-jobs osm-obj classads) [:state :status] (analysis-status classads))))))

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
    (let [osm-cl      (osm-client)
          osm-objects (running-jobs osm-cl)
          osm-uuids   (map #(:uuid (:state %)) osm-objects)
          classads    (concat 
                        (parallel-func queue osm-uuids)
                        (parallel-func history osm-uuids))]
      (log/debug classads)
      (-> osm-objects
        (update-osm-objects classads)
        (log/debug)
        (post-osm-updates osm-cl)))
    (recur)))
