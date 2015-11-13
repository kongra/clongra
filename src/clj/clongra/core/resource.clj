;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

;; SAFE RESOURCE MANAGEMENT

(defrecord ^:private
    Resource [value cleanup ^Thread hook]

    clojure.lang.IDeref
    (deref [this] value))

(prefer-method print-method clojure.lang.IDeref clojure.lang.IRecord)
(prefer-method print-method clojure.lang.IDeref java.util.Map)
(prefer-method print-method clojure.lang.IDeref clojure.lang.IPersistentMap)


(defn Resource?
  [obj]
  (and (map? obj)
       (:value obj) (:cleanup obj) (instance? Thread (:hook obj))))


(defn create-resource!
  "Creates a resource and performs all the instrumentation. An
  explicit usage highly discouraged."
  [value cleanup]
  (io!
    (let [runnable (fn []
                     (try (cleanup value)
                          (catch Throwable e
                            (.printStackTrace e))))

          hook (Thread. ^Runnable runnable)]
      (.. Runtime getRuntime (addShutdownHook hook))
      (Resource. value cleanup hook))))


(defn close-resource!
  "Closes the resource. An explicit usage highly discouraged."
  [resource]
  (io!
    (try ((:cleanup resource) (:value resource))
         (.. Runtime getRuntime (removeShutdownHook (:hook resource)))

         (catch Throwable e
           (.printStackTrace e)))))


(defmacro defresource
  [name expr cleanup]
  `(let [v# (def ~name)]
     (when (.hasRoot v#)
       ;; ALREADY DEFINED
       (let [r# (.get v#)]
         (when (Resource? r#)
           (close-resource! r#))))

     (def ~name (create-resource! ~expr ~cleanup))))


(defn ns-unmap-safely!
  "Works like clojure.core/ns-unmap but is Resource-aware. Closes a
  Resource if it is a mapping value for sym in ns."
  [ns sym]
  (io!
    (let [value (var-root (ns-resolve ns sym))]
      (when (Resource? value)
        (close-resource! value))

      (ns-unmap ns sym))))


(defn remove-ns-safely!
  "Works like clojure.core/remove-ns but is Resource-aware. Closes any
  Resource that is a value in the mapping of the namespace depicted by
  sym."
  [sym]
  (io!
    (when (find-ns sym)
      (doseq [v (vals (ns-map sym))]
        (when (instance? clojure.lang.Var v)
          (let [value (var-root v)]
            (when (Resource? value)
              (close-resource! value)))))

      (remove-ns sym))))
