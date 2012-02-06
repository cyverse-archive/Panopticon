(ns panopticon.core
  (:gen-class)
  (:require [clojure-commons.osm :as osm]
            [clojure-commons.props :as props]
            [clojure-commons.clavin-client :as cl]
            [clojure-commons.file-utils :as ft]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]
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
  []
  (try
    (let [query {"$or" [{"state.status" RUNNING} 
                        {"state.status" SUBMITTED}
                        {"state.status" IDLE}]}]
      (:objects (json/read-json (osm/query (osm-client) query))))
    (catch java.lang.Exception e
      (log/warn e)
      [])))

(defn post-osm-updates
  [osm-objects]
  (doseq [osm-object osm-objects]
    (let [new-state (:state osm-object)
          osm-id    (:object_persistence_uuid osm-object)]
      (if (nil? osm-id)
        (log/info (str "OSM state" new-state)))
      (osm/update-object (osm-client) osm-id new-state))))

(defn classad-lines
  [classad-str]
  (into [] (filter #(not (re-find #"^--" %)) (-> classad-str (string/split #"\n")))))

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

(defn constraint [uuid] (str "IpcUuid ==\"" uuid "\""))

(defn- run-history
  [uuid]
  (let [const   (constraint uuid)
        results (sh/sh "condor_history" "-l" "-constraint" const)]
    (log/warn (str "condor_history -l --constraint " const))
    (log/info (str "Exit Code: " (:exit results)))
    (log/info (str "stderr: " (:err results)))
    (log/info (str "stdout: " (:out results)))
    (classad-maps (string/trim (:out results)))))

(defn history
  [uuids]
  (log/warn (count uuids))
  (into [] (flatten (pmap run-history uuids))))

(defn- run-queue
  [uuid]
  (let [const   (constraint uuid)
        results (sh/sh "condor_q" "-long" "-constraint" const)]
    (log/warn (str "condor_q -long -constraint " const))
    (log/info (str "Exit Code: " (:exit results)))
    (log/info (str "stderr: " (:err results)))
    (log/info (str "stdout: " (:out results)))
    (classad-maps (string/trim (:out results)))))

(defn queue
  [uuids]
  (log/warn (count uuids))
  (into [] (flatten (pmap run-queue uuids))))

(defn condor-rm
  [dag-id]
  (let [cmd ["condor_rm" dag-id]
        results (apply sh/sh cmd)]
    (log/warn cmd)
    (log/warn (str "Exit Code: " (:exit results)))
    (log/warn (str "stderr: " (:err results)))
    (log/warn (str "stdout: " (:out results)))))

(defn transfer
  [source-dir output-dir]
  (when (ft/exists? source-dir)
    (let [exect    "/usr/local/bin/filetool"
          results (sh/sh exect "-source" source-dir "-destination" output-dir)]
      (log/warn (str exect " -source " source-dir " -destination " output-dir))
      (log/warn (str "Exit Code: " (:exit results)))
      (log/warn (str "stderr: " (:err results)))
      (log/warn (str "stdout: " (:out results))))))

(defn rm-dir
  [dir-path]
  (let [dobj (clojure.java.io/file dir-path)]
    (try
      (org.apache.commons.io.FileUtils/deleteDirectory dobj)
      (catch java.lang.Exception e
        (log/warn e)))))

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

(defn de-job-status
  [classad]
  (let [condor-status  (job-status classad)
        exit-by-signal (boolize (get classad "ExitBySignal"))
        exit-code      (get classad "ExitCode")]
    (cond
      (and (= condor-status COMPLETED) exit-by-signal)       FAILED
      (and (= condor-status COMPLETED) (not= exit-code "0")) FAILED
      (= condor-status HELD)    FAILED
      (= condor-status SUBERR)  FAILED
      (= condor-status REMOVED) FAILED
      :else                     condor-status)))

(defn- status-matches? [[job-key job] status] (= (:status job) status))
(defn- seq-jobs [obj] (seq (:jobs (:state obj))))

(defn no-jobs-queued?
  [uuid]
  (let [queued (queue [uuid])]
    (and
      (= (count queued) 1)
      (nil? (first queued)))))

(defn analysis-submitted? 
  [obj] 
  (every? #(status-matches? % SUBMITTED) (seq-jobs obj)))

(defn analysis-completed? 
  [obj] 
  (and
    (every? #(status-matches? % COMPLETED) (seq-jobs obj))
    (no-jobs-queued? (:uuid obj))))

(defn analysis-running? 
  [obj] 
  (any? #(status-matches? % RUNNING) (seq-jobs obj)))

(defn analysis-failed? 
  [obj] 
  (and
    (any? #(status-matches? % FAILED) (seq-jobs obj))
    (no-jobs-queued? (:uuid obj))))

(defn analysis-held? 
  [obj] 
  (any? #(status-matches? % HELD) (seq-jobs obj)))

(defn analysis-status
  [osm-obj]
  (cond
    (analysis-failed? osm-obj)    FAILED
    (analysis-completed? osm-obj) COMPLETED
    (analysis-submitted? osm-obj) SUBMITTED
    (analysis-running? osm-obj)   RUNNING
    (analysis-held? osm-obj)      HELD
    :else                         IDLE))

(defn classads-for-osm-object
  [osm-object all-classads]
  (let [osm-uuid (:uuid (:state osm-object))]
    (into [] (filter #(= (get % "IpcUuid") osm-uuid) all-classads))))

(defn osm-job-for-classad
  [classad osm-object]
  (let [cl-job-id (keyword (get classad "IpcJobId"))
        osm-jobs (:jobs (:state osm-object))]
    [cl-job-id (get osm-jobs cl-job-id)]))


(defn jobs-maps
  [osm-obj classads]
  (apply merge (into [] (for [classad classads]
                          (let [[job-id job] (osm-job-for-classad classad osm-obj)]
                            (log/warn (str "JOB ID: " job-id "\tOSMID: " (:object_persistence_uuid osm-obj)))
                            {job-id (assoc job :status (de-job-status classad)
                                               :exit-code (get classad "ExitCode")
                                               :exit-by-signal (get classad "ExitBySignal"))})))))

(defn update-jobs
  [osm-obj classads]
  (let [osm-jobs     (jobs-maps osm-obj classads)
        all-osm-jobs (:jobs (:state osm-obj))
        merged-jobs  (apply merge (into [] (flatten [all-osm-jobs osm-jobs])))]
    (assoc-in osm-obj [:state :jobs] merged-jobs)))

(defn update-osm-objects
  [osm-objects all-classads]
  (into [] (filter 
             #(not (nil? %)) 
             (for [osm-obj osm-objects]
               (let [classads (classads-for-osm-object osm-obj all-classads)]
                 (if (> (count classads) 0)
                   (let [updated-jobs (update-jobs osm-obj classads)
                         a-status     (analysis-status updated-jobs)]
                     (assoc-in updated-jobs [:state :status] a-status))))))))

(defn cleanup
  [osm-objects]
  (doseq [osm-object osm-objects]
    (let [jstatus (get-in osm-object [:state :status])
          dag-id  (get-in osm-object [:state :dag_id])
          ldir    (get-in osm-object [:state :condor-log-dir])
          wdir    (get-in osm-object [:state :working_dir])
          odir    (get-in osm-object [:state :output_dir])]
      (cond
        (= jstatus HELD)
        (do (condor-rm dag-id)
          (transfer wdir odir)
          (transfer ldir odir)
          (rm-dir ldir))
        
        (= jstatus FAILED)
        (do (transfer ldir odir)
          (rm-dir ldir))
        
        (= jstatus COMPLETED)
        (do (transfer wdir odir)  
          (transfer ldir odir)
          (rm-dir ldir)))))
  osm-objects)

(defn filter-classads [classads] (into [] (filter #(contains? % "IpcUuid") classads)))

(defn -main
  [& args]
  (def zkprops (props/parse-properties "panopticon.properties"))
  (def zkurl (get zkprops "zookeeper"))
  
  (log/info "Starting up. Reading configuration from Zookeeper.")
  
  (cl/with-zk
    zkurl
    (when (not (cl/can-run?))
      (log/warn "THIS APPLICATION CANNOT RUN ON THIS MACHINE. SO SAYETH ZOOKEEPER.")
      (log/warn "THIS APPLICATION WILL NOT EXECUTE CORRECTLY.")
      (System/exit 1))
    
    (reset! props (cl/properties "panopticon")))
  
  (log/info "Done reading configuration from Zookeeper.")
  (log/info (str "OSM Client: " (osm-client)))
  (loop []
    (let [osm-objects (running-jobs)]
      (when (> (count osm-objects) 0)
        (let [osm-uuids (into [] (map #(:uuid (:state %)) osm-objects))
              classads  (filter-classads (concat (queue osm-uuids) (history osm-uuids)))]
          (-> osm-objects
            (update-osm-objects classads)
            (cleanup)
            (post-osm-updates)))))
    (recur)))
