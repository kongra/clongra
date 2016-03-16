;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

(defn mapconcat
  "Behaves like a Common Lisp mappend function. Concatenates the
   results of mapping f over coll. Generated sequence is lazy."
  [f coll] (apply concat (map f coll)))


(defn mapfn
  "Returns a lazy sequence of functions call values on x."
  [x & fs] (map (fn [f] (f x)) fs))


(defn mark-last
  "Takes a sequence (e0 e1 ... en) and returns (false false ... true)
  or (false false ...) if the argument is infinite. Lazy."
  [coll]
  (if-not (seq coll)
    '()
    (let [[_ & xs] coll]
      (if-not (seq xs)
        '(true)
        (cons false (lazy-seq (mark-last xs)))))))


(defn assoc-conj ;:- coll v -> {k, coll v} -> k -> v -> {k, coll v}
  "Adds v to a collection that is a value for k in m. Uses empty-coll
  when no collection for k in m."
  [m k v empty-coll]
  (assoc m k (conj (get m k empty-coll) v)))


(defn dissoc-nils
  "Dissociates any keys for which m is nil."
  [m]
  (loop [new-map m
	 ks (keys m)]
    (if-let [k (first ks)]
      (recur (if (m k) new-map (dissoc new-map k)) (rest ks))

      new-map)))


(defn seq-to-map
  "Transforms the sequence coll into a map where the i-th mapping is
  (keyf coll[i]) → (valf coll[i]). The default keyf and valf are
  clojure.core/identity."
  ([keyf valf coll]
     (when (seq coll)
       (->> coll
            (reduce (fn [m obj] (assoc! m (keyf obj) (valf obj)))
                    (transient {}))
            (persistent!))))

  ([valf coll] (seq-to-map clojure.core/identity valf coll))

  ([coll] (seq-to-map clojure.core/identity coll)))


(defn map-to-map
  "Transforms the map m into a new map with keys transformed by keyf
  and values transformed by valf. The default keyf and valf are
  clojure.core/identity."
  ([keyf valf m]
     (->> m
          (reduce-kv (fn [m k v] (assoc! m (keyf k) (valf v)))
                     (transient {}))
          (persistent!)))

  ([valf m] (map-to-map clojure.core/identity valf m))

  ([m] (map-to-map clojure.core/identity m)))


(defn reduce-by
  "@See Clojure Programming pp. 119"
  [key-fn f init coll]
  (reduce
   (fn [summaries x]
     (let [k (key-fn x)]
       (assoc summaries k (f (summaries k init) x))))

   {} coll))


(defn reduce-by-in
  "@See Clojure Programming pp. 121"
  [keys-fn f init coll]
  (reduce (fn [summaries x]
            (let [ks (keys-fn x)]
              (assoc-in summaries ks
                        (f (get-in summaries ks init) x))))
          {} coll))


(let [GET*-NOTFOUND (gensym 'GET)]
  (defn get*
    "An iterative version of clojure.core/get.

    Treats the given map as a directed graph with a maximum number of
    outgoing edges equal to 1. Iteratively visits keys (nodes) of the
    map (graph) until either the unary key-pred is false for key
    (node) or a cycle is found. Returns the last node (key)."
    ([map key key-pred not-found]
       (if-not (key-pred key)
         not-found

         (let [last-value (get map key GET*-NOTFOUND)]
           (if (= last-value GET*-NOTFOUND)
             not-found

             (loop [key last-value
                    visited #{}]

               (if-not (key-pred key)
                 key

                 (let [last-value (get map key GET*-NOTFOUND)]
                   (if (or (= last-value GET*-NOTFOUND)
                           (visited last-value))
                     key

                     (recur last-value (conj visited key))))))))))

    ([map key not-found]
       (get* map key (return true) not-found))

    ([map key]
       (get* map key nil))))


(defn subsequence
  "Returns a subsequence between 0-based indices start
  (inclusive) and end (exclusive)."
  ([coll start]
     (drop start coll))
  ([coll start end]
     (take (- end start) (subsequence coll start))))


(defn vec-remove
  "Returns a vector that is a result of removing i-th element from the
  vector v."
  [i v]
  (vec (concat (subvec v 0 i) (subvec v (inc i)))))


(defmacro dorange
  "Assumes bindings to be a vector [i start end]. Executes body with i
   carrying subsequent longs from start (inclusively) to end
   (exclusively)."
  [bindings & body]
  (assert (vector? bindings))
  (assert (= 3 (count bindings)))
  (let [[i start end] bindings]
    `(let [n# (long ~end)]
       (loop [~i (long ~start)]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))


(defn rotate
  "Takes a sequence and left rotates it n steps. If n is
  negative, the sequence is rotated right. Executes in O(n) time.

  The implementation was created by Sean Devlin, @see
  http://vimeo.com/11446902"
  [n coll]
  (if (empty? coll)
    '()
    (let [shift (mod n (count coll))]
      (concat (drop shift coll) (take shift coll)))))


(defn asserted-distinct
  "Works like clojure.core/distinct (large parts of the body are
  copied), but instead of ommitting, asserts false when a duplicate
  element is found."
  [coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                 ((fn [[f :as xs] seen]
                    (when-let [s (seq xs)]
                      (if (contains? seen f)
                        (assert false (str "A duplicate found: " f))

                        (cons f (step (rest s) (conj seen f))))))

                  xs seen)))]

    (step coll #{})))


(defn iterate-with
  "Works like clojure.core/iterate, but uses additional arguments
  from colls when calling f."
  ([f x coll & colls]
     (letfn [(iw [f x coll & colls]
               (when (and (seq coll) (every? seq colls))
                 (let [x (apply f x (first coll)
                                (map first colls))]
                   (cons x
                         (lazy-seq (apply iw f x (next coll)
                                          (map next colls)))))))]

       (cons x (apply iw f x coll colls))))

  ([f x coll]
     (letfn [(iw [f x coll]
               (when (seq coll)
                 (let [x (f x (first coll))]
                   (cons x
                         (lazy-seq (iw f x (next coll)))))))]

       (cons x (iw f x coll)))))


(defmacro doreduce
  "Iterates with i from 0 to n-1 (inclusively). In every step
  performs expr and sets it's value to the result. The initial
  result value is init."
  [[i n result init] expr]
  `(let [n# (int ~n)]
     (loop [~i (int 0) ~result ~init]
       (if (< ~i n#)
	 (recur (unchecked-inc ~i) ~expr)

	 ~result))))


(defn reuse-cons
  "An implementation of a canonical reuse-cons Common Lisp
  function."
  ([x y x-y]
     (reuse-cons x y x-y =))
  ([x y x-y equality-test]
     (if (and (equality-test x (first x-y))
	      (equality-test y (rest x-y)))
       x-y
       (cons x y))))


(defn replace-tree
  "Works like clojure.core/replace but treats the second argument
  like a tree rather than a flat collection. An analog to Common
  Lisp sublis."
  [smap tree]
  (map (fn [element]
	 (if (sequential? element)
	   (replace-tree smap element)
	   element))
       (replace smap tree)))


;; FREQUENCY DISTRIBUTION

(defn freqdist-inc
  "Takes a frequency distribution and increases it's frequency for the
  given item."
  ([fdist item]
     (assoc fdist item (inc (get fdist item 0))))

  ([fdist item & items]
     (reduce freqdist-inc (freqdist-inc fdist item) items)))


(defn freqdist
  "Takes a sequence of elements and returns a frequency distribution
  for it's elements. The keys nor values in the distribution are not
  sorted."
  [coll]
  (reduce freqdist-inc {} coll))


(defn freqdist-keys
  "Returns the frequency distribution keys, sorted by their related
  frequency."
  [fdist]
  (sort (comparator (fn [k1 k2] (> (fdist k1) (fdist k2))))
        (keys fdist)))


(defn freqdist-entries
  "Returns a sequence of 2-element vectors representing the entries in
  the frequency distribution. The sequence is sorted descending by the
  frequencies."
  [fdist]
  (->> fdist
       freqdist-keys
       (map #(vector % (fdist %)))))


(defn freqdist-total-count
  "Returns the summary count of all entries in the frequency
  distribution."
  [fdist]
  (reduce + (vals fdist)))


;; POWERSET

(declare ebit-test ebit-length N')

(defn- powerset-generator
  [indexed-coll n]
  (->> indexed-coll
       (take (ebit-length n))
       (filter (fn [p] (ebit-test n (pair-first p))))
       (map pair-second)))


(defn nth-in-powerset
  "Returns an n-th element of a powerset of a collection. Works for
  n : T <: Long and for (possibly) infinite collections. That's why
  for the finite colls there are no range checks for n."
  [coll n]
  (powerset-generator (indexed' coll) n))


(defn powerset
  "Returns a powerset for a possibly infinite coll."
  [coll]
  (let [indexed-coll (indexed' coll)]
    (->> (N' 1)
         (map #(powerset-generator indexed-coll %))
         (take-while seq)
         (cons '()))))


(declare **)

(defn powerset-eseq
  "Returns an enhanced version of the powerset. Every element of the
  returned collection is subjected to a transformation using enthtrans
  (identity by default)."
  ([enthtrans coll]
     (let [n (len coll)
           result (->> coll
                       powerset
                       (map enthtrans)
                       (with-enth #(enthtrans (nth-in-powerset coll %))))]
       (if (+∞? n)
         (infinite result)

         (with-delayed-len (** 2 n) result))))

  ([coll]
     (powerset-eseq clojure.core/identity ;; enthtrans
                    coll)))


;; CARTESIAN PRODUCT

(defn nth-in-cartesian-product
  "Returns the n-th element of the cartesian product of the given
  colls (coll_0 ... coll_m-1). The indices within the returned tuple
  are established according to the following formula:

  i_0   = (n / |coll_1||coll_2| ... |coll_m-1|) mod |coll_0|
  i_1   = (n /         |coll_2| ... |coll_m-1|) mod |coll_1|
  ...
  i_m-2 = (n / |coll_m-1|) mod |coll_m-2|
  i_m-1 = n mod |coll_m-1|"
  [n & colls]
  (let [lens (map len colls)]
    ;; We use ∞ aware operators cause len might return ∞.
    (if (some ∞zero? lens)
      ;; If at least one collection is empty - product is nil and nth
      ;; of nil is also nil. We cound also raise an error here, but
      ;; the former behavior corresponds with the default
      ;; clojure.core/nth behavior for nil collections.
      nil

      (do
        ;; Range check ...
        (when (< n 0)
          (terror IndexOutOfBoundsException "Negative n " n))

        ;; ... but only when there is no infinity we check the right
        ;; border.
        (when (and (not (some +∞? lens)) (>= n (reduce *' lens)))
          (terror IndexOutOfBoundsException "n out of range" n))

        (let [divs (->> (reverse lens)
                        (iterate-with ∞*' 1)
                        next
                        (cons 1)
                        reverse
                        next)

              index-gen (fn [d l] (∞mod (integer (∞div n d)) l))
              indices   (map index-gen divs lens)]

          (map (fn [i coll] (enth coll i)) indices colls))))))


(defn cartesian-product
  "Returns a cartesian product of the colls. The number of colls must
  (and is assumed to) be finite."
  [& colls]
  (apply combo/cartesian-product colls))


(defn cartesian-product-eseq
  "Returns an enhanced cartesian product of colls. Every element of
  the returned collection is subjected to a transformation using
  enthtrans (identity by default)."
  ([enthtrans colls]
     (let [n (reduce ∞*' (map len colls))
           prod (->> colls
                     (apply cartesian-product)
                     (map enthtrans)
                     (with-enth #(enthtrans (apply nth-in-cartesian-product
                                                   %
                                                   colls))))]
       (if (+∞? n)
         (infinite prod)

         (with-len-value n prod))))

  ([colls]
     (cartesian-product-eseq clojure.core/identity ;; enthtrans
                             colls)))


;; (def s1 (take 2 (consymbs 'a)))
;; (def s2 (take 3 (consymbs 'b)))
;; (def s3 (take 2 (consymbs 'c)))
;; (def s4 (take 4 (consymbs 'd)))
;; (def s5 (take 1 (consymbs 'e)))

;; (defn- teścior
;;   [n & colls]
;;   (let [cprod (take n (apply cartesian-product colls))]
;;     (doseq [[i elem] (indexed cprod)]
;;       (let [nelem (apply nth-in-cartesian-product i colls)]
;;         (println nelem)
;;         (when-not (= elem nelem)
;;           (println "Problem dla n " i " " elem " i " nelem))))))


;; PERMUTATIONS

(defn permutations
  "Returns a collection of permutations (lexicographical) of coll."
  [coll]
  (combo/permutations coll))


(declare factoradic)

(defn nth-permutation
  "Returns n-th permutation (lexicographical) of the given coll."
  [coll n]
  (let [v (if (vector? coll) coll (vec coll))
        lehmer-code (factoradic n)

        ;; lehmer-code must be supplemented with 0-s to match the
        ;; length of v
        zeros-count (- (count v) (count lehmer-code))
        lehmer-code (concat (repeat zeros-count 0) lehmer-code)

        gen (fn [[_ v] i] (pair (nth v i) (vec-remove i v)))]

    (->> (iterate-with gen (pair nil v) lehmer-code)
         next
         (map pair-first))))


;; (defn- test-nth-permutations
;;   [coll]
;;   (doseq [[i perm] (indexed (permutations coll))]
;;     (when (zero? (mod i 1000))
;;       (println "Testing" i))

;;     (assert (= perm (nth-permutation coll i))
;;             (tstr "Problem with " perm " and " i))))


(declare factorial')

(defn permutations-eseq
  "Returns an enhanced version of permutations. Every element of the
  returned collection is subjected to a transformation using enthtrans
  (identity by default)."
  ([enthtrans coll]
     (->> coll
          permutations
          (map enthtrans)
          (with-delayed-len (factorial' (len coll)))
          (with-enth #(enthtrans (nth-permutation coll %)))))

  ([coll]
     (permutations-eseq clojure.core/identity ;; enthtrans
                        coll)))


;; MISC. ALGORITHMS

(declare breadth-first-tree-seq)

(defn breadth-first-Σ*
  "Takes a finite alphabet Σ and returns a lazy infinite Σ*
  collection of strings over the alphabet (including ε) using a
  breadth-first strategy.
  E.g. for Σ = {a, b} returns
  (() (a) (b) (a a) (a b) (b a) (b b) (a a a) (a a b) (a b a)
         (a b b) ...).

  Beware a non-optimistic breadth-first memory consumption."
  [Σ]
  (breadth-first-tree-seq '()
			  (fn [node]
			    (map #(lazy-cat node (list %)) Σ))))


(def SMALL-LETTERS
  '[a b c d e f g h i j k l m n o p q r s t u v w x y z])

(def SMALL-LETTERS-POLISH
  '[a ą b c ć d e ę f g h i j k l ł m n ń o ó p r s t u w x y z])
