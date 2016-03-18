;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-20

(in-ns 'clongra.core)

(defn truth
  "Maps the obj to itself if it is not nil, to true otherwise."
  [obj]
  (or obj true))


(defmacro ref= [x y] `(jclongra.core.Ident/refeq ~x ~y))

(defmacro boolean-not [b] `(jclongra.core.Booleans/not ~b))


(defn MapEntry-to
  ([f ^java.util.Map$Entry entry]
     (f (.getKey entry) (.getValue entry)))

  ([f]
     (fn [entry]
       (MapEntry-to f entry))))


(defn array-seq
  [& elements] elements)


(defn not-nil?
  "If x is not nil, returns it. Nil otherwise. Trivial yet
  convenient predicate for tassert. An alias for identity."
  [x] x)


(defn atomic?
  "Returns true if x is not sequential. (atomic? nil) ==> true.
  An analog to Common Lisp atom function, that returns false only
  for conses."
  [x] (not (sequential? x)))


(defn singleton?
  "If the coll is a 1-element collection, it's returned, nil
  otherwise."
  [coll]
  (when (and (seq coll) (not (next coll)))
    coll))


(defn singleton-or-empty?
  "If the coll is an empty or 1-element collection, it's
  returned, nil otherwise."
  [coll]
  (if-not (seq coll)
    (truth coll)

    (when (not (next coll))
      coll)))


(defn lazy?
  "Answers true if the given object is a lazy collection."
  [obj]
  (when (instance? clojure.lang.LazySeq obj)
    obj))


(defn array?
  [obj]
  (when-let [c (class obj)]
    (when (.isArray c)
      obj)))


(defn Byte?
  "Returns b iff it's a java.lang.Byte instance."
  [b]
  (when (instance? Byte b)
    b))


(defn Integer?
  "Returns n iff is's an Integer instance"
  [n]
  (when (instance? Integer n)
    n))


;; (defn ^Integer to-Integer
;;   "Converts the passed argument to an Integer."
;;   [n]
;;   (if (Integer? n)
;;     n

;;     (Integer. (int n))))

(defprotocol CoercableToMathematicalInteger
  (coerce-to-integer [this]))


(defn integer
  "Converts the passed value into a mathematical integer."
  [x]
  (cond (or (instance? Long x)
            (instance? clojure.lang.BigInt x)
            (instance? Integer x)
            (instance? java.math.BigInteger x)
            (instance? Byte x)
            (instance? Short x))
        x

        (or (instance? java.lang.Double x)
            (instance? java.lang.Float x))
        (.longValue ^Number x)

        (instance? java.math.BigDecimal x)
        (.toBigInteger ^java.math.BigDecimal x)

        (instance? clojure.lang.Ratio x)
        (.bigIntegerValue ^clojure.lang.Ratio x)

        (instance? Character x)
        (int (.charValue ^Character x))

        :else (coerce-to-integer x)))


(defn iseqable?
  "Returns truth of obj iff the obj may be coerced to a sequence
  (clojure.lang.ISeq) by (seq obj)."
  [obj]
  (when (or (instance? clojure.lang.ASeq obj)
            (instance? clojure.lang.LazySeq obj)
            (instance? clojure.lang.Seqable obj)
            (nil? obj)
            (instance? java.lang.Iterable obj)
            (.isArray (class obj))
            (instance? java.lang.CharSequence obj)
            (instance? java.util.Map obj))

    (truth obj)))


(defn ideref?
  [obj]
  (instance? clojure.lang.IDeref obj))


(defn ensure-deref
  [obj]
  (if (ideref? obj) (deref obj) obj))


(defn sequentialize
  "If the object is a sequence, return seq on it, (obj)
  otherwise. Fast wrapper of objects into a sequence. For nil
  returns an empty sequence."
  [obj]
  (cond (nil? obj) '()
	(iseqable? obj) (seq obj)
	:else (vector obj)))


(defn with-atoms-to-symbols
  "Converts all atomic elements of the object (possibly a
  collection) to symbols."
  [obj]
  (cwalk/postwalk
   (fn [x] (if (atomic? x)
	     (if-not (symbol? x) (symbol (tstr x)) x)
	     x))
   obj))


(defmacro return
  "Creates a function that called returns the obj"
  [obj]
  `(jclongra.core.Return. (fn [] ~obj)))


(defmacro return-seq
  "Works like (return ...), but requires the arguments to be an
  iseqable? collection. The returned object is iseqable? itself."
  [coll]
  `(jclongra.core.ReturnSeq. (fn [] (tassert iseqable? ~coll))))


(defmacro unsafe-return-seq
  [coll]
  `(jclongra.core.ReturnSeq. (fn [] ~coll)))


(defmacro returns
  "Works like a list constructor but wraps every item with (return ...)."
  [& items]
  (let [items (map (fn [it] `(return ~it)) items)]
    `(jclongra.core.Returns. (list ~@items))))


(defmacro delay-seq
  "Works like (delay ...), but requires the arguments to be an
  iseqable? collection. The returned object is iseqable? itself."
  [coll]
  `(clongra.core.DelaySeq. (fn [] (tassert iseqable? ~coll))))


(defmacro unsafe-delay-seq
  [coll]
  `(clongra.core.DelaySeq. (fn [] ~coll)))


(defmacro delays
  "Works like a list constructor but wraps every item with (delay ...)."
  [& items]
  (let [items (map (fn [it] `(delay ~it)) items)]
    `(clongra.core.Delays. (list ~@items))))


(declare multiarg-class-dispatch)

(defmulti non
  "A multi-purpose multimethod to generate negative predicates.
  When called with no arguments returns a function that returns
  nil for any arguments."
  {:arglists '([]
		 [npred]
		   [npred pred])}

  multiarg-class-dispatch)


(defmethod non [clojure.lang.IFn clojure.lang.IFn]
  [npred pred]
  (fn [obj]
    (when (and (not (npred obj))
	       (pred obj))
      (truth obj))))


(defmethod non clojure.lang.IFn
  [npred]
  (fn [arg]
    (when-not (npred arg)
      (truth arg))))


(defmethod non :noarg
  []
  (fn [& _] nil))


(defmacro in-situ
  "Invokes print to print object, then returns the object."
  [print obj]
  `(let [obj# ~obj]
     (do (~print obj#) obj#)))


(defn comp+
  "An extended version of composition function. Allows no args to
  be passed to it (returns identity on that occassion). Based on:
  http://blog.fogus.me/2010/08/18/
                           monkeying-with-clojures-comp-function/"
  ([] identity)

  ([f & fs] (apply comp f fs)))


;; COMPARATORS

(defn keyword-comparator
  "Returns the comparator based on the predicate applied to
  keyword values of it's arguments."
  [k pred]
  (comparator (fn [x y] (pred (k x) (k y)))))


(defn asc
  "Returns a comparator that uses the consecutive key functions (like
  in clojure.core/sort-by) to sort the coll ascending."
  [& keyfns]
  (fn [x y]
    (or (->> keyfns
             (map (fn [f] (compare (f x) (f y))))
             (filter (complement zero?))
             first)
        0)))


(defn desc
  "Returns a comparator that uses the consecutive key functions (like
  in clojure.core/sort-by) to sort the coll descending."
  [& keyfns]
  (fn [x y]
    (or (->> keyfns
             (map (fn [f] (compare (f y) (f x))))
             (filter (complement zero?))
             first)
        0)))


;; PAIRS

(defn ^jclongra.core.Pair pair
  "Returns a pair of elements."
  [x y] (jclongra.core.Pair/of x y))


(defn pair?
  [x]
  (when (instance? jclongra.core.Pair x)
    x))


(defn pair-first
  [^jclongra.core.Pair p] (.first p))


(defn pair-second
  [^jclongra.core.Pair p] (.second p))


;; TURNABLE SWITCHES

(defmacro defswitch
  ([name on?]
     `(dyndef ~name (boolean ~on?)))

  ([name]
     `(defswitch ~name true)))


(declare dynval dynset!)

(defn switch-on!
  [switch]
  (dynset! switch true))


(defn switch-off!
  [switch]
  (dynset! switch false))


(defn on?
  [switch]
  (dynval switch))


(defn off?
  [switch]
  (not (on? switch)))


(defmacro switching-on
  [switches & body]
  (assert (vector? switches) "The switches must be a vector form.")
  (assert (seq switches)) "The switches must not be empty."
  (let [clause (fn [switch] `(~switch true))
        clauses (->> (map clause switches)
                     (apply concat)
                     vec)]

    `(dynbinding ~clauses ~@body)))


(defmacro switching-off
  [switches & body]
  (assert (vector? switches) "The switches must be a vector form.")
  (assert (seq switches)) "The switches must not be empty."
  (let [clause (fn [switch] `(~switch false))
        clauses (->> (map clause switches)
                     (apply concat)
                     vec)]

    `(dynbinding ~clauses ~@body)))


;; DISPATCH ROUTINES

(defn first-type-dispatch
  [x & more]
  (class x))


(defn multiarg-class-dispatch
  "An effective multi-argument dispatcher function."
  ([] :noarg)

  ([x] (class x))

  ([x y] (vector (class x) (class y)))

  ([x y & args] (vec (map class (cons x (cons y args))))))


;; AN EXTENSIBLE seq

(defprotocol TSeqable
  "An extensible version of clojure.core/seq. The procedure tseq may
  be called with and additional argument (e.g. a comparator comp when
  tseqing maps)."

  (tseq [this] [this arg]))


(defn- map-to-seq
  "Transforms a map to a sequence sorting keys with the comp
  (clojure.core/compare by default)."
  ([comp m]
     (->> (keys m)
          (sort comp)
          (map #(vector % (m %)))))

  ([m] (map-to-seq clojure.core/compare m)))


(extend-protocol TSeqable
  java.util.Iterator
  (tseq [it] (clojure.lang.IteratorSeq/create it))

  java.util.Enumeration
  (tseq [e] (clojure.lang.IteratorSeq/create
             (jclongra.core.CollectionUtils/iterator e)))

  java.lang.Object
  (tseq [obj] (seq obj))

  nil
  (tseq [& _] nil)

  clojure.lang.PersistentTreeMap
  (tseq
    ([m] (seq m)) ;; assume sorted here

    ([m comp] (map-to-seq comp m)))

  java.util.SortedMap
  (tseq
    ([m] (seq m)) ;; assume sorted here

    ([m comp] (map-to-seq comp m)))

  java.util.Map
  (tseq
    ([m] (map-to-seq m))

    ([m comp] (map-to-seq comp m)))

  jclongra.core.Returns
  (tseq [rs] (map (fn [r] (r)) (.items rs)))

  jclongra.core.Delays
  (tseq [ds] (map force (.items ds))))


;; INDEXED COLLECTIONS PROTOCOL

(defprotocol Indexable
  (indexed [coll]
    "Returns a sequence of pairs ((index-1 value-1) ... (index-n
   value-n)) for the (not neccessarily) sequential collection."))


(extend-protocol Indexable
  clojure.lang.APersistentMap
  (indexed [m] (seq m))

  jclongra.core.LinkedSeq
  (indexed [ls] (map pair (iterate inc 0) ls))

  java.util.List
  (indexed [l] (map pair (iterate inc 0) l))

  java.util.Map
  (indexed [m] (map #(pair % (get m %)) (keys m)))

  java.util.Set
  (indexed [s] (map pair s s))

  java.lang.String
  (indexed [s] (indexed (seq s)))

  nil
  (indexed [_] '()))


(defn indexed'
  [coll]
  (map pair (N') coll))


(defn indexes-of
  "Returns a collection of indexes of all coll elements that pass the
  given predicate."
  [pred coll]
  (->> coll
       indexed
       (filter #(pred (pair-second %)))
       (map pair-first)))


(defn index-of
  "Returns an index of the first occurence of a collection element
  that passes the given predicate."
  [pred coll]
  (first (indexes-of pred coll)))


(defn positions [pred coll]
  "Returns the sequence of position (indices) of the collection
  elements for which pred is true."
  (for [[i v] (indexed coll) :when (pred v)] i))


(declare subsequence)

(defn find-idx
  "Returns the index of the first occurence of a value in the
  sequential collection. Uses a specified predicate to match the
  value. Nil means the search failure.

  Optional parameters start and end narrow the search to a subset
  of the sequence. However the returned index is relative to the
  entire sequence (like in CL position function)."
  ([pred coll]
     (let [pred (if (ifn? pred) pred #(= pred %))]
       (loop [i 0 coll coll]
         (if (seq coll)
           (if (pred (first coll))
             i

             (recur (inc i) (next coll)))

           nil))))

  ([pred coll start]
     (when-let [idx (find-idx pred (subsequence coll start))]
       (+ idx start)))

  ([pred coll start end]
     (when-let [idx (find-idx pred (subsequence coll start end))]
       (+ idx start))))


(defn find-idx'
  "Works like find-idx but uses ' operators."
  ([pred coll]
     (let [pred (if (ifn? pred) pred #(= pred %))]
       (loop [i (Long/valueOf 0)
              coll coll]
         (if (seq coll)
           (if (pred (first coll))
             i

             (recur (inc' i) (next coll)))

           nil))))

  ([pred coll start]
     (when-let [idx (find-idx' pred (subsequence coll start))]
       (+' idx start)))

  ([pred coll start end]
     (when-let [idx (find-idx' pred (subsequence coll start end))]
       (+' idx start))))


;; MEMBERSHIP PROTOCOL AND STUFF

(defn seq-member?
  "Tests for a membership of a given value to a specified
  sequential collection. Optionally uses a comparator function
  (defaults to =)."
  ([coll value]
     (seq-member? coll value =))
  ([coll value comp]
     (some #(comp value %) coll)))


(defprotocol WithMembers
  (member? [coll value] [coll value comp]
    "Returns the value iff it belongs to a collection. Optionally uses
    a comparator where possible"))


(extend-protocol WithMembers
  nil
  (member?
    ([coll value] nil)
    ([coll value comp] nil))

  java.util.Set
  (member?
    ([s value] (when (.contains s value) (truth value)))
    ([s value comp] (when (seq-member? s value comp) (truth value))))

  jclongra.core.ReturnSeq
  (member?
    ([rs value] (member? (rs) value))
    ([rs value comp] (member? (rs) value comp)))

  jclongra.core.DelaySeq
  (member?
    ([ds value] (member? (ds) value))
    ([ds value comp] (member? (ds) value comp)))

  ;; ALL OTHER (ASSUMED SEQUENTIAL COLLECTIONS)
  java.lang.Object
  (member?
    ([coll value] (when (seq-member? coll value) (truth value)))
    ([coll value comp] (when (seq-member? coll value comp) (truth value)))))


;; BOX/UNBOX

(defn ^jclongra.core.Box box
  "Creates and returns a boxed value."
  [value] (jclongra.core.Box/of value))


(defn unbox
  "Unboxes a boxed value."
  [^jclongra.core.Box box] (.get box))


;; RESETTABLE MEMOIZATION

(def NO-MEMO-PRED (return true))


(defn memo
  "A resettable version of clojure.core/memoize. @see memo-reset!
  Takes an optional unary predicate that says whether or not the
  result is to be cached. Allows tracing the memo cache/non-cache
  hits."
  ([f {:keys [pred trace-hits]
       :or   {pred       NO-MEMO-PRED
              trace-hits false}
       :as   options}]
     (validate-legal-keys [pred trace-hits] options)
     (jclongra.core.Memo. f pred (boolean trace-hits)))

  ([f]
     (memo f {})))


(defn memo-reset!
  "Resets the cache of the memoized function. @see memo."
  ([^jclongra.core.Memo memo clear-hits?]
     (.resetAll memo (flag-value clear-hits?)))

  ([memo]
     (memo-reset! memo :clear-hits)))


(defn memo-reset-key!
  [^jclongra.core.Memo memo key]
  (.reset memo key))


(defn memo-size
  "Returns the number items (keys) stored in the cache"
  [^jclongra.core.Memo memo]
  (.cacheSize memo))


(defn redef-as-memo
  "Redefines the named function to be a memoized one."
  {:arglists '([f {:keys [pred trace-hits]
                   :or   {pred       NO-MEMO-PRED
                          trace-hits false}
                   :as   options}]

                 [f])}
  ([f options]
     (let [[_ v] (tassert not-nil? (ns-entry f)
                          (tstr "Can't find namespace Var for " f))

           m (meta v)
           name (with-meta (:name m) m)]

       (intern *ns* name (memo (.getRawRoot ^clojure.lang.Var v)
                               options))))

  ([f]
     (redef-as-memo f {})))


(def dynamic-memos (atom {}))

(defn redef-as-dynamic-memo
  "Redefines the named function to be a memoized one.  The function
  MUST be defined as the dynamic one."
  {:arglists '([f {:keys [pred trace-hits]
                   :or   {pred       NO-MEMO-PRED
                          trace-hits false}
                   :as   options}]

                 [f])}
  ([f options]
     (let [[s v] (tassert not-nil? (ns-entry f)
                          (tstr "Can't find namespace Var for " f))

           _ (assert (.isDynamic ^clojure.lang.Var v)
                     (tstr "The function " s " MUST be dynamic."))

           raw (.getRawRoot ^clojure.lang.Var v)]

       (swap! dynamic-memos assoc v (return (memo raw options)))))

  ([f]
     (redef-as-dynamic-memo f {})))


(defmacro memoizing
  "Executes the body within a dynamic memoization context for the
  given functions."
  [fs & body]
  (assert (vector? fs))
  (assert (seq fs))

  (let [clause
        (fn [f]
          `(~f ((tassert not-nil? (@dynamic-memos (ns-resolve *ns* '~f))
                         (tstr "The procedure " '~f
                               " was not redefined as a dynamic memo.")))))

        clauses (apply concat (map clause fs))]

    `(binding [~@clauses] ~@body)))


;; (defn ^:dynamic fib
;;   [n]
;;   (if (< n 2) n (+' (fib (dec n)) (fib (- n 2)))))

;; (redef-as-dynamic-memo fib)


(defn local-memo
  "Returns a non-resettable, thread-unsafe memo function to be used in
  local contexts."
  ([f pred]
     (let [mem (java.util.HashMap.)]
       (fn [& args]
         (if (.containsKey mem args)
           (.get mem args)

           (let [ret (apply f args)]
             (when (pred ret)
               (.put mem args ret))

             ret)))))

  ([f] (local-memo f NO-MEMO-PRED)))


;; TYPE HINTING

(defn with-hint
  "Adds a :tag type hint to the passed obj. If type is a symbol
  quoted multiple number of times, unquotes it
  first. E.g. '''''String and 'String mean the same."
  [type obj]
  (let [t (or (last (flatten type)) type)]
    (when-not (symbol? t)
      (println (tstr "Suspiciously hint type " type
                     " is not a symbol but "
                     (class type)
                     " that cannot be unquoted to a symbol.\n"
                     "Called from "(current-stack-frame 1) ".")))

    (vary-meta obj assoc :tag t)))


;; HINTED FAST ARRAY ACCESS

(defmacro deep-aget
  "Works like clojure.core/aget but uses type hints to speed
  thing up.  Thanks to Christophe Grand:
  http://clj-me.cgrand.net/category/interop/"

  ([type array idx]
     `(aget ~(with-hint type array) ~idx))

  ([type array idx & idxs]
     `(let [a# (aget ~(with-hint 'objects array) ~idx)]
	(deep-aget ~type a# ~@idxs))))


(declare java-primitive-singular)

(defmacro deep-aset
  "Works like clojure.core/aget but uses type hints to speed
  thing up.  Thanks to Christophe Grand:
  http://clj-me.cgrand.net/category/interop/"
  [type array & idxsv]
  (let [type (last (flatten type))
	[v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (java-primitive-singular type)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~'objects ~array ~@idxs)
		       array)
        a-sym (with-meta (gensym "a") {:tag type})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~idx ~v))))


;; META PRESERVING ON COLLECTIONS

(defmacro preserving-meta
  "Executes the body and returns the result with meta equal to (meta obj)."
  [obj & body]
  `(with-meta (do ~@body) (meta ~obj)))


(defmacro merging-meta
  "Executes the body and returns the result with meta being a result
  of merging (meta result) and (meta obj)."
  [obj & body]
  `(let [result# (do ~@body)]
     (with-meta result# (merge (meta result#) (meta ~obj)))))


;; VAR PROXIES

(defmacro set-var-proxy!
  [proxy obj]
  `(let [proxy# ~proxy]
     (.set proxy# (second (ns-entry ~obj)))))

(set-var-proxy! jclongra.core.Proxies/tseq tseq)
(set-var-proxy! jclongra.core.Proxies/tstr tstr)
