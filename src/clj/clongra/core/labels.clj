;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-20

(in-ns 'clongra.core)

(defn label
  "Returns an interned symbol for a label described by it's name and
  (optionally) namespace."
  ([name]
     (jclongra.core.Labels/intern name))

  ([ns name]
     (symbol ns (clojure.core/name (label name)))))


(defn- eval-label-name
  [name]
  (cond (string? name) name
        (symbol? name) (str name)
        :else (str (eval name))))


(defmacro defl
  "Defines a value for the given label"
  ([name value]
     `(def ~(label (eval-label-name name)) ~value))

  ([attr-map name value]
     `(def ~(with-meta (label (eval-label-name name)) attr-map) ~value)))


(defmacro lv
  ([name]
     (with-meta (label (eval-label-name name))
       (meta &form)))

  ([ns name]
     (with-meta (label (eval-label-name ns)
                       (eval-label-name name))
       (meta &form))))


(defmacro the
  [& args]
  (->> args
       (map eval-label-name)
       (interpose " ")
       (apply str)))


(defmacro The
  [& args]
  (str *ns* "/"
       (->> args
            (map eval-label-name)
            (interpose " ")
            (apply str))))


(defmacro a
  [& args]
  `(the ~@args))


(defmacro A
  [& args]
  `(The ~@args))


(defmacro an
  [& args]
  `(the ~@args))


(defmacro An
  [& args]
  `(The ~@args))


(defmacro l
  [& args]
  `(the ~@args))


(defmacro L
  [& args]
  `(The ~@args))
