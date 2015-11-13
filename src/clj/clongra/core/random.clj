;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-04-22

(in-ns 'clongra.core)

;; RANDOM NUMBERS GENERATORS

(defn ^java.util.Random secure-random
  "@see java.security.SecureRandom"
  []
  (java.security.SecureRandom.))


;; RANDOM NUMBERS ABSTRACTION

(def ^:dynamic ^java.util.Random *random*
  (secure-random))

(defmacro with-random
  [random & body]
  `(binding [*random* ~random] ~@body))


;; GENERATION

(defn- ^Long rand-long!
  [^java.util.Random random ^Number bound]
  (long (* (.nextDouble random) (.doubleValue bound))))


(defn- ^java.math.BigInteger rand-big-integer!
  [^java.util.Random random ^java.math.BigInteger bound]
  (let [n (.bitLength bound)]
    (loop []
      (let [r (java.math.BigInteger. n random)]
        (if (< (.compareTo r bound) 0)
          r
          (recur))))))


(defn- rand-big-int!
  [^java.util.Random random ^clojure.lang.BigInt bound]
  (if-let [bipart (.bipart bound)]
    (clojure.lang.BigInt/fromBigInteger (rand-big-integer! random bipart))

    (rand-long! random (.lpart bound))))


(defn rand-integer!
  "Returns a pseudo-randomly generated integral value in range
  [0, bound)."
  ([^java.util.Random random bound]
     (cond (instance? Long bound)
           (rand-long! random bound)

           (or (instance? Integer bound)
               (instance? Short   bound)
               (instance? Byte    bound))
           (.nextInt random (.intValue ^Number bound))

           (instance? clojure.lang.BigInt bound)
           (rand-big-int! random bound)

           (instance? java.math.BigInteger bound)
           (rand-big-integer! random bound)

           :else (terror IllegalArgumentException "Illegal bound" bound)))

  ([bound]
     (rand-integer! *random* bound)))


(defn rand-integers!
  "Returns an infinite sequence of pseudo-randomly generated integral
  value, every one of them in [0, bound)."
  ([random bound]
     (->> bound
          (rand-integer! random)
          return
          repeatedly))

  ([bound]
     (rand-integers! *random* bound)))


;; ALGORITHMS

(defn take-rand!
  "Returns a collection of n elements taken randomly (possibly
  multiple times) out of the coll. The coll must be finite.

  Returned collection is enhanced (with len value)."
  ([random n coll]
     (let [size (len coll)]
       (when (+∞? size)
         (terror IllegalArgumentException "Infinite coll not allowed."))

       (->> (rand-integers! random size)
            (take n)
            (map #(enth coll %))
            (with-len-value n))))

  ([n coll]
     (take-rand! *random* n coll)))


(defn take-rand-distinct!
  "Returns a collection of n distinct elements taken randomly out of
  the coll. The coll must be finite.

  Returned collection is enhanced (with len value)."
  ([random n coll]
     (let [size (len coll)]
       (when (+∞? size)
         (terror IllegalArgumentException "Infinite coll not allowed."))

       (let [;; take at most size elements
             n (if (> n size) size n)]
         (->> (rand-integers! random size)
              distinct
              (take n)
              (map #(enth coll %))
              (with-len-value n)))))

  ([n coll]
     (take-rand-distinct! *random* n coll)))


(defn shuffle-with-random!
  "Shuffles randomly the passed collection. Optionally uses a
  source of randomness."
  ([coll]
     (shuffle-with-random! *random* coll))

  ([random ^java.util.Collection coll]
     (when coll
       (let [al (java.util.ArrayList. coll)]
         (java.util.Collections/shuffle al random)
         (clojure.lang.RT/vector (.toArray al))))))
