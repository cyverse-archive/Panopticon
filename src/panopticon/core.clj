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

;Converts a string to a boolean.
(def boolize #(boolean (Boolean. %)))

;Kinda like (every?)
(defn any? [pred coll] ((comp not not-any?) pred coll))

(defn osm-client [] (osm/create (osm-url) (osm-coll)))
(defn job-status [classad-map] (get JOBSTATUS (get classad-map "JobStatus")))

(defn running-jobs
  "Queries the OSM for a list of jobs that are in the
   Running, Submitted, or Idle states."
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
  "POSTs updated objects to the OSM. Input is a sequence of maps
   in the form:

   {:object_persistence_uuid string
    :state {}}

   In other words, you need to have the UUID associated with the
   doc in the OSM and the new state of the object."
  [osm-objects]
  (doseq [osm-object osm-objects]
    (let [new-state (:state osm-object)
          osm-id    (:object_persistence_uuid osm-object)]
      (if (nil? osm-id)
        (log/info (str "OSM state" new-state)))
      (osm/update-object (osm-client) osm-id new-state))))

(defn classad-lines
  "Takes a string containing all of the classad information for a job
   and returns a list of strings. It splits the input string on \n and
   filters out any lines that begin with --."
  [classad-str]
  (into [] (filter #(not (re-find #"^--" %)) (-> classad-str (string/split #"\n")))))

(defn classad-maps
  "Transforms the output of either (queue) or (history) into
   a sequence of maps. Basically, the classads are parse so
   everything to the left of the first = is the key and
   everything to the right is the value."
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
  "Runs condor_history looking for single uuid, parses the output,
   and returns a sequence of maps created by (classad-maps)."
  [uuid]
  (let [const   (constraint uuid)
        results (sh/sh "condor_history" "-l" "-constraint" const)]
    (log/warn (str "condor_history -l --constraint " const))
    (log/info (str "Exit Code: " (:exit results)))
    (log/info (str "stderr: " (:err results)))
    (log/info (str "stdout: " (:out results)))
    (classad-maps (string/trim (:out results)))))

(defn history
  "Iterates over the list of uuids and calls condor_history for each one.
   Uses pmap to call condor_history. Returns a list of maps created by
   (classad-maps)."
  [uuids]
  (log/warn (count uuids))
  (into [] (flatten (pmap run-history uuids))))

(defn- run-queue
  "Runs condor_q looking for a single uuid, parses the output,
   and returns a sequence of maps created by (classad-maps)."
  [uuid]
  (let [const   (constraint uuid)
        results (sh/sh "condor_q" "-long" "-constraint" const)]
    (log/warn (str "condor_q -long -constraint " const))
    (log/info (str "Exit Code: " (:exit results)))
    (log/info (str "stderr: " (:err results)))
    (log/info (str "stdout: " (:out results)))
    (classad-maps (string/trim (:out results)))))

(defn queue
  "Iterates over the list of uuids and calls condor_q for each one.
   Uses pmap to call condor_q. Returns a list of maps created by
   (classad-maps)."
  [uuids]
  (log/warn (count uuids))
  (into [] (flatten (pmap run-queue uuids))))

(defn condor-rm
  "Calls condor_rm on a dag."
  [dag-id]
  (let [cmd ["condor_rm" dag-id]
        results (apply sh/sh cmd)]
    (log/warn cmd)
    (log/warn (str "Exit Code: " (:exit results)))
    (log/warn (str "stderr: " (:err results)))
    (log/warn (str "stdout: " (:out results)))))

(defn transfer
  "Calls filetool to transfer a directory of files into iRODS."
  [source-dir output-dir]
  (when (ft/exists? source-dir)
    (let [exect    "/usr/local/bin/filetool"
          results (sh/sh exect "-source" source-dir "-destination" output-dir)]
      (log/warn (str exect " -source " source-dir " -destination " output-dir))
      (log/warn (str "Exit Code: " (:exit results)))
      (log/warn (str "stderr: " (:err results)))
      (log/warn (str "stdout: " (:out results))))))

(defn rm-dir
  "Uses Apache Commons IO to recursively delete a directory. Does not play nice
   with NFS mountpoints."
  [dir-path]
  (let [dobj (clojure.java.io/file dir-path)]
    (try
      (org.apache.commons.io.FileUtils/deleteDirectory dobj)
      (catch java.lang.Exception e
        (log/warn e)))))

(defn job-failed?
  "Takes in a single classad-map generated from (classad-maps)
   and tells you whether the job failed or not. This is not a straight-forward
   translation of the Condor job states, it uses ExitBySignal and ExitCode
   to determine if a job in the COMPLETED state actually failed."
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
  "Takes in single classad-map generated from (classad-maps)
   and translates the Condor status into a DE job state."
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
  "Calls (queue) and determines if there are no more jobs remaining
   in the queue for a DAG."
  [uuid]
  (let [queued (queue [uuid])]
    (and
      (= (count queued) 1)
      (nil? (first queued)))))

(defn analysis-submitted?
  "Takes in a map retrieved from the OSM and returns true
   if it represents an analysis in the SUBMITTED state."
  [obj] 
  (every? #(status-matches? % SUBMITTED) (seq-jobs obj)))

(defn analysis-completed? 
  [obj]
  "Takes in a map retrieved from the OSM and returns true
   if it represents an analysis in the COMPLETED state.
   Takes into account whether or not there are any more
   jobs in the queue, which will hopefully avoid any race
   conditions."
  (and
    (every? #(status-matches? % COMPLETED) (seq-jobs obj))
    (no-jobs-queued? (:uuid obj))))

(defn analysis-running?
  "Takes in a map retrieved from the OSM and returns true
   if it represents an analysis in the RUNNING state."
  [obj] 
  (any? #(status-matches? % RUNNING) (seq-jobs obj)))

(defn analysis-failed? 
  "Takes in a map retrieved from the OSM and returns true
   if it represents an analysis in the FAILED state.
   Takes into account whether or not there are any more
   jobs in the queue, which will hopefully avoid any race
   conditions."
  [obj] 
  (and
    (any? #(status-matches? % FAILED) (seq-jobs obj))
    (no-jobs-queued? (:uuid obj))))

(defn analysis-held? 
  "Takes in a map retrieved from the OSM and returns true
   if it represents an analysis in the FAILED state."
  [obj] 
  (any? #(status-matches? % HELD) (seq-jobs obj)))

(defn analysis-status
  "Takes in a map retrieved from the OSM and returns which
   state the analysis is in."
  [osm-obj]
  (cond
    (analysis-failed? osm-obj)    FAILED
    (analysis-completed? osm-obj) COMPLETED
    (analysis-submitted? osm-obj) SUBMITTED
    (analysis-running? osm-obj)   RUNNING
    (analysis-held? osm-obj)      HELD
    :else                         IDLE))

(defn classads-for-osm-object
  "Takes in an osm-object and a list of classad maps and filters
   the classad maps down to just those that are pertinent to the
   analysis represented by the osm-object."
  [osm-object all-classads]
  (let [osm-uuid (:uuid (:state osm-object))]
    (into [] (filter #(= (get % "IpcUuid") osm-uuid) all-classads))))

(defn osm-job-for-classad
  "Takes in a classad map and a osm-object and returns a vector-tuple
   that lists the job id and the job map from the osm-objects' 
   :jobs sub-map."
  [classad osm-object]
  (let [cl-job-id (keyword (get classad "IpcJobId"))
        osm-jobs (:jobs (:state osm-object))]
    [cl-job-id (get osm-jobs cl-job-id)]))


(defn jobs-maps
  "Takes in an osm-object and a list of classad maps and returns
   a new version of the :jobs sub-map with all of the jobs updated
   with info from the classads. Called by (update-jobs) below."
  [osm-obj classads]
  (apply merge 
    (into [] 
      (for [classad classads]
        (let [[job-id job] (osm-job-for-classad classad osm-obj)]
          (log/warn (str "JOB ID: " job-id "\tOSMID: " (:object_persistence_uuid osm-obj)))
          {job-id (assoc job 
                    :status (de-job-status classad)
                    :exit-code (get classad "ExitCode")
                    :exit-by-signal (get classad "ExitBySignal"))})))))

(defn update-jobs
  "Takes in an osm-object and a list of classad maps, updates the state
   of the jobs in the osm-object, and returns a new version of the 
   osm-object with the job changes applied."
  [osm-obj classads]
  (let [osm-jobs     (jobs-maps osm-obj classads)
        all-osm-jobs (:jobs (:state osm-obj))
        merged-jobs  (apply merge (into [] (flatten [all-osm-jobs osm-jobs])))]
    (assoc-in osm-obj [:state :jobs] merged-jobs)))

(defn update-osm-objects
  "Takes in a list of osm-objects and a list of classad maps. Returns
   a list of osm-objects that have been updated with information from
   the classads that apply to them. Incidentally, this is where the
   (analysis-status) function is called to determine the overall state
   of an analysis."
  [osm-objects all-classads]
  (into [] 
    (filter 
      #(not (nil? %)) 
      (for [osm-obj osm-objects]
        (let [classads (classads-for-osm-object osm-obj all-classads)]
          (if (> (count classads) 0)
            (let [updated-jobs (update-jobs osm-obj classads)
                  a-status     (analysis-status updated-jobs)]
              (assoc-in updated-jobs [:state :status] a-status))))))))

(defn cleanup
  "Takes in a list of osm-objects and performs clean up actions based on the
   status of the analysis they represent."
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
          (comment (rm-dir ldir)))
        
        (or (= jstatus COMPLETED) 
            (= jstatus FAILED))
        (do (transfer wdir odir)  
          (transfer ldir odir)
          (comment (rm-dir ldir))))))
  osm-objects)

(defn filter-classads
  "Simple function that filters a list of classad maps so
   it only contains classad maps for jobs that were run through
   the DE."
  [classads] 
  (into [] (filter #(contains? % "IpcUuid") classads)))

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
