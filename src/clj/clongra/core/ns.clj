;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

(defn symbol-value
  "Resolves and returns the value of the symbol or nil if unable
  to bind."
  [symbol namespace]
  (try (.. clojure.lang.Compiler
	   (resolveIn namespace symbol false))

       (catch java.lang.Exception e nil)))


(defn with-ns-name
  "Assumes a named object. Returns a symbol consisting of (name
  named) / and the name of the namespace. If no namespace present
  in the named, the name of the current namespace is taken."
  [^clojure.lang.Named named]
  (if (namespace named)
    named

    (let [cnstr (if (symbol? named) symbol keyword)]
      (cnstr (str *ns* "/" (name named))))))


(defn intern-ns!
  "Interns all interned elements of interned-ns into ns (*ns* by
  default).  The strategy describes the way of refering to
  interned-ns elements, e.g.  ns-interns, ns-publics (default)."
  ([ns interned-ns strategy]
     (io!
       (let [assert-ns
             (fn [nsname]
               (if-let [n (find-ns (symbol (tstr nsname)))]
                 n
                 (throw
                  (new IllegalArgumentException
                       (tstr "Unrecognized namespace "
                             nsname ".")))))

             ns (assert-ns (tstr ns))
             interned-ns (assert-ns (tstr interned-ns))]

         (doseq [[sym v] (strategy interned-ns)]
           (intern ns (with-meta sym (meta v)) (deref v))))))

  ([interned-ns] (intern-ns! *ns* interned-ns ns-publics)))


(declare not-nil?)

(defn var-root
  "Returns a raw root of the clojure.lang.Var v, nil if none present."
  [^clojure.lang.Var v]
  (when (and v (.hasRoot v))
    (.getRawRoot v)))


(defn ns-entries
  "Returns a sequence of entries [symbol var] for the given obj in the
  namespace ns. The entries-accessor is the source of mappings
  (clojure.core/ns-interns by default)."
  ([ns obj entries-accessor]
     (->> (entries-accessor ns)
          (map (fn [[s v :as p]]
                 (when (= obj (if (var? v) (var-root v) v))
                   p)))

          (filter not-nil?)))

  ([ns obj] (ns-entries ns obj ns-interns))

  ([obj] (ns-entries *ns* obj)))


(declare singleton-or-empty?)

(defn ns-entry
  "Returns an entry [symbol var] for the given obj in the namespace
  ns. Nil if none found. The entries-accessor is the source of
  mappings (clojure.core/ns-interns by default)."
  {:arglists '([ns obj entries-accessor]
                 [ns obj]
                   [obj])}
  [& args]
  (let [ents (apply ns-entries args)]
    (assert (singleton-or-empty? ents)
            (str "The obj has multiple ns-entries. Unable to select one."))

    (first ents)))


(defmacro make-private!
  [arg]
  (let [evarg (eval arg)
        name  (with-meta arg {:private true})]

    `(def ~name ~arg)))
