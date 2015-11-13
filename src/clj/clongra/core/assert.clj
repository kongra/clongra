;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

(defmacro TODO
  [& what]
  (let [what (str "TODO: " (apply tstr-spaced what))]
    `(throw
      (jclongra.NotImplementedException. ~what))))


(defmacro UNSUPPORTED
  [& why]
  (let [why (apply tstr-spaced why)]
    `(throw (UnsupportedOperationException. ~why))))


;; ASSERTIONS

(defmacro tassert
  "When called with a single parameter pred returns a function
  that takes an argument and asserts the result of calling pred
  on this argument, then returns the argument.

  When called with additional argument, simply calls the
  assertion on pred on the argument."
  ([pred]
     (when *assert*
       `(fn [x#]
	  (when-not (~pred x#)
	    (throw (AssertionError.
                    (tstr-spaced "Assert failed:" '~pred "on" x#))))
	  x#)))

  ([pred x]
     (when *assert*
       `(let [x# ~x]
	  (when-not (~pred x#)
	    (throw (AssertionError.
                    (tstr-spaced "Assert failed:" '~pred "on" x#))))
	  x#)))

  ([pred x msg]
     (when *assert*
       `(let [x# ~x]
	  (when-not (~pred x#)
	    (throw (AssertionError. (tstr ~msg))))
	  x#))))


(defn tpred
  "Takes a predicate function and returns a tassert-capable
  predicate, i.e. a predicate that takes an argument and returns
  this argument when pred on this argument is true."
  [pred]
  (fn [arg] (when (pred arg) (truth arg))))


(declare ns-entry var-root)

(defn declare-tpred
  "Declares the named function to be a tpred one. "
  [f]
  (let [[_ v] (ns-entry f)
	m (meta v)
	name (with-meta (:name m) m)]

    (intern *ns* name (tpred (var-root v)))))


;; ERRORS

(def ValueError jclongra.ValueError)
(def TypeError  jclongra.TypeError)
(def StateError jclongra.StateError)

(defmacro terror
  "Causes the system to throw an exception with the given args. If the
  first argument is a Throwable derivative class, uses the specific
  class as the exception type."
  {:arglists '([& more] [throwable-class & more])}
  [& args]
  (let [[eclass & rest] args]
    (if-not (symbol? eclass)
      `(do (throw (RuntimeException. (tstr-spaced ~@args))) nil)

      (let [eveclass (eval eclass)]
        (if-not (isa? eveclass Throwable)
          `(do (throw (RuntimeException. (tstr-spaced ~@args))) nil)

          (let [econstr (symbol (str (.getName ^Class eveclass) "."))]
            `(do (throw (~econstr (tstr-spaced ~@rest))) nil)))))))


(def ^:dynamic *trecoveries* {})

(let [valid-trecovery-spec?
      (tpred #(and (vector? %)
		   (seq %)
		   (<= (count %) 2)))

      valid-bindings-shape?
      (tpred #(and (vector? %) (even? (count %))))

      process-spec
      (fn [spec]
	(let [[where location] (tassert valid-trecovery-spec?
					spec)]
	  (if location
	    `(pair ~where '~location)

	    where)))

      process-bindings
      (fn [bindings]
	(->> (partition 2 bindings)
	     (map (fn [[spec handler]]
		    (list (process-spec spec) handler)))
	     (reduce concat)))]

  (defmacro terror-recovering
    "Executes the body of expressions within the set of bindings
    for terror recoveries. Sample usage:

    (terror-recovering [[where location] handler] body)
    (terror-recovering [[where] handler] body).

    where is a name of a valid procedure in which we want to use
    the provided error recovery handler. Location is an
    additional symbolic (e.g. keyword) tag within where. handler
    is a name of the handler procedure.

    Empty bindings means inhibiting all previously defined
    recovery handlers."

    [bindings & body]
    (tassert valid-bindings-shape? bindings)
    (let [assoc-args (process-bindings bindings)]
      (if (seq assoc-args)
	`(binding [*trecoveries* (assoc *trecoveries*
				   ~@assoc-args)]
	   ~@body)

	`(binding [*trecoveries* {}]
	   ~@body))))


  (defmacro terror-at
    "Works like terror but provides location specification and
    parameters for potential terror recovery."
    {:arglists '([[where location] params & args]
		   [[where] params & args])}

    [spec params & args]
    (let [spec (process-spec spec)]
      (if (sequential? spec)
	`(let [spec# ~spec]
	   (if-let [handler# (*trecoveries* spec#)]
	     (handler# ~@params)

	     (let [where# (pair-first spec#)]
	       (if-let [handler# (*trecoveries* where#)]
		 (handler# ~@params)

		 (terror ~@args)))))

	`(let [spec# ~spec]
	   (if-let [handler# (*trecoveries* spec#)]
	     (handler# ~@params)

	     (terror ~@args)))))))
