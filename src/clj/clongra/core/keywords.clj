;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-20

(in-ns 'clongra.core)

;; DESTRUCTURING KEYVALS AND MAPS ARGUMENTS

(declare sequentialize)

(defmacro with-keyargs
  "An anaphoric macro that uses the map-destructurable collection
  (either a map or a keyvals sequence) to perform a keyword-based
  destructuring to symbols mentioned in the clauses part and to
  execute the body of expressions within these new bindings.

  clause := symbol | (symbol default-value)"

  [coll clauses & body]

  (assert (vector? clauses) "Clauses must be a vector.")
  (assert (seq clauses) "Clauses must not be empty.")

  (let [clauses (map sequentialize clauses)
        symbols (map first clauses)

        destruct {:keys (vec symbols)}

        default (->> clauses
                     (filter #(= 2 (count %)))
                     (map (fn [[s v]] `(~s (or ~s ~v))))
                     (reduce concat))]

    `(let [coll# ~coll
           ~destruct (cond (map? coll#) coll#
                           (set? coll#) (seq-to-map (return true) coll#)
                           :else (apply hash-map coll#))
           ~@default]

       ~@body)))


(defn keywordize
  "Transforms the argument to a keyword if possible."
  [obj]
  (if (keyword? obj) obj (keyword (str obj))))


(defmacro validate-legal-keys
  [legal-keys map]
  (assert (seq legal-keys) "The legal-keys must not be empty.")
  (let [legal-keys (->> legal-keys
                        (clojure.core/map keywordize)
                        set)]

    `(let [legal-keys# ~legal-keys
           map# ~map]
       (doseq [k# (keys map#)]
         (assert (legal-keys# k#)
                 (str "The key " k# " in " ~map " is illegal. "
                      "The legal ones are " legal-keys#)))

       map#)))
