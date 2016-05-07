;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created  2009-10-02
;; Reworked 2014-03-24

(in-ns 'clongra.core)

;; TURNING ON/OFF

(def ^:private logged-ids (atom #{}))

(defn log-on!
  "Turns on logging for either the passed ids or the current
  namespace. Ids may be arbitrary objects."
  ([]
     (log-on! *ns*))

  ([id & ids]
     (apply swap! logged-ids conj id ids)))


(defn log-off!
  "Turns on logging for either the passed ids or the current
  namespace. Ids may be arbitrary objects."
  ([]
     (log-off! *ns*))

  ([id & ids]
     (apply swap! logged-ids disj id ids)))


(defn logged?
  ([] (logged? *ns*))

  ([id]
     (@logged-ids id)))


;; INTERNALS

(defonce ^:private logger
  (delay (org.apache.logging.log4j.LogManager/getLogger "clongra.core")))

(defonce ^:private logger-agent (agent nil))

(declare transaction-running?)

(defn- invoke-log!
  [^org.apache.logging.log4j.Level level ^String msg]
  (if (transaction-running?)
    (do (send-off logger-agent
               (fn [& _]
                 (ignoring-all-exceptions
                   (.log ^org.apache.logging.log4j.Logger @logger
                         level msg))))
        nil)

    (.log ^org.apache.logging.log4j.Logger @logger
         level msg)))


;; (defn- ^String log-message
;;   [level msgs]
;;   (let [now (.toString (joda) "Y-MM-d H:mm:ss,SSS")
;;         ^StackTraceElement frame (current-stack-frame 3)
;;         file (.getFileName frame)
;;         line (.getLineNumber frame)
;;         th (.. Thread currentThread getName)]

;;     (str now
;;          (when level (str " " level))
;;          " [" th "] " file ":" line " " (apply tstr-spaced msgs))))

(defn- ^String log-message
  [level msgs]
  (str (when level (str level " ")) (apply tstr-spaced msgs)))


;; LOGGING LEVELS

(defn log-fatal!
  [& more]
  (when (logged?)
    (invoke-log! org.apache.logging.log4j.Level/FATAL
                 (log-message "FATAL" more))))


(defn log-error!
  [& more]
  (when (logged?)
    (invoke-log! org.apache.logging.log4j.Level/ERROR
                 (log-message "ERROR" more))))


(defn log-warn!
  [& more]
  (when (logged?)
    (invoke-log! org.apache.logging.log4j.Level/WARN
                 (log-message "WARN" more))))


(defn log-info!
  [& more]
  (when (logged?)
    (invoke-log! org.apache.logging.log4j.Level/INFO
                 (log-message "INFO" more))))


(defn log-debug!
  [& more]
  (when (logged?)
    (invoke-log! org.apache.logging.log4j.Level/DEBUG
                 (log-message "DEBUG" more))))


(defn log-trace!
  [& more]
  (when (logged?)
    (invoke-log! org.apache.logging.log4j.Level/TRACE
                 (log-message "TRACE" more))))


;; PRINTLN

(defn log-println!
  [& more]
  (when (logged?)
    ;; When printing we abandon the STM transactional considerations
    ;; and do not use logger-agent.
    (println (log-message nil more))))


;; LOG ON FOR CORE

(log-on!)
