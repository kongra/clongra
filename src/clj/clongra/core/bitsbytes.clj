;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

(defn long-bit-length
  "Returns the bit length of an integral <: Long, either positive or
  not."
  [n]
  (tassert integer? n)
  (jclongra.core.Bits/bitLength (long n)))


(defn long-bits
  "Returns the bits of a positive long value. Starts with the least
  significant bit."
  [n]
  (tassert integer? n)
  (jclongra.core.Bits/longBits (long n)))


(defn bits-to-integral
  "Returns the integral value of the collection of bits starting
  with the least significant bit."
  ([coll +op *op]
     (->> coll
          (map (fn [i val] (*op val (** 2 i))) (N'))
          (reduce +op)))

  ([coll] (bits-to-integral coll +' *')))


;; ENHANCED BIT OPERATORS PROTOCOL

(defprotocol WithEnhancedBitOps
  (ebit-length [n])
  (ebit-test   [x n])

  ;; ... with more to come here if needed
  )


(extend-protocol WithEnhancedBitOps
  java.lang.Byte
  (ebit-length [n]   (jclongra.core.Bits/bitLength (.longValue n)))
  (ebit-test   [x n] (clojure.lang.Numbers/testBit (.longValue x) (int n)))

  java.lang.Short
  (ebit-length [n]   (jclongra.core.Bits/bitLength (.longValue n)))
  (ebit-test   [x n] (clojure.lang.Numbers/testBit (.longValue x) (int n)))

  java.lang.Integer
  (ebit-length [n]   (jclongra.core.Bits/bitLength (.longValue n)))
  (ebit-test   [x n] (clojure.lang.Numbers/testBit (.longValue x) (int n)))

  java.lang.Long
  (ebit-length [n]   (jclongra.core.Bits/bitLength (.longValue n)))
  (ebit-test   [x n] (clojure.lang.Numbers/testBit (.longValue x) (int n)))

  clojure.lang.BigInt
  (ebit-length [n]
    (if-let [bp (.bipart n)]
      (.bitLength bp)
      (jclongra.core.Bits/bitLength (.lpart n))))

  (ebit-test [x n]
    (if-let [bp (.bipart x)]
      (.testBit bp (int n))
      (clojure.lang.Numbers/testBit (.lpart x) (int n))))

  java.math.BigInteger
  (ebit-length [n]   (.bitLength n))
  (ebit-test   [x n] (.testBit x (int n))))


;; clongra.core.Bytes

(defn ^jclongra.core.Bytes bytes-of
  [^bytes value]
  (jclongra.core.Bytes/valueOf value))


(defn ^jclongra.core.Bytes bytes-of-size
  [size]
  (jclongra.core.Bytes/ofSize (int size)))


(defn ^bytes bytes-value
  [^jclongra.core.Bytes bytes]
  (.getBytes bytes))


(defn ^bytes byte-array-of-size
  [size]
  (jclongra.core.Bytes/arrayOfSize (int size)))


;; TO-/FROM BYTES CONVERSION

(def ^:private NO-BYTES (byte-array-of-size 0))

(deftype BytesConverters [to-bytes from-bytes])

(def ^:private bytes-converters (atom {})) ;; tag → BytesConverters

(defn- no-bytes?
  [^bytes bytes]
  (or (nil? bytes) (zero? (alength bytes))))


(defn- to-bytes-wrapper
  [f]
  (fn [obj] (if obj (f obj) NO-BYTES)))


(defn- from-bytes-wrapper
  [f]
  (fn [bytes] (when-not (no-bytes? bytes) (f bytes))))


(defn def-bytes-converters
  [[to-bytes from-bytes] & types]
  (let [to-bytes   (to-bytes-wrapper   to-bytes)
        from-bytes (from-bytes-wrapper from-bytes)]

    (doseq [t types]
      (swap! bytes-converters assoc t (BytesConverters. to-bytes from-bytes)))))


(defn to-bytes
  ([type value]
     ((to-bytes type) value))

  ([type]
     (if-let [^BytesConverters convs (@bytes-converters type)]
       (.to-bytes convs)

       (terror "Unsupported type " type))))


(defn from-bytes
  ([type bytes]
     ((from-bytes type) bytes))

  ([type]
     (if-let [^BytesConverters convs (@bytes-converters type)]
       (.from-bytes convs)

       (terror "Unsupported type " type))))


;; COMMON TO-/FROM BYTES CONVERTERS

;; ARRAY OF byte/Byte

(def-bytes-converters [identity identity] :bytes)

(def-bytes-converters [(fn [^jclongra.core.Bytes bytes] (bytes-value bytes))
                       (fn [^bytes bytes] (bytes-of bytes))]
  jclongra.core.Bytes)


;; String

(def-bytes-converters [(fn [^String s]    (.getBytes s   ENCODING))
                       (fn [^bytes bytes] (String. bytes ENCODING))]

  :str java.lang.String)


;; PRIMITIVE TYPES

(def-bytes-converters [(fn [b]     (jclongra.core.Bits/booleanToBytes b))
                       (fn [bytes] (jclongra.core.Bits/getBoolean bytes 0))]

  :boolean :bool java.lang.Boolean)


(def-bytes-converters [(fn [b]     (jclongra.core.Bits/byteToBytes b))
                       (fn [bytes] (jclongra.core.Bits/getByte bytes 0))]

  :byte java.lang.Byte)


(def-bytes-converters [(fn [s]     (jclongra.core.Bits/shortToBytes s))
                       (fn [bytes] (jclongra.core.Bits/getShort bytes 0))]

  :short java.lang.Short)


(def-bytes-converters [(fn [c]     (jclongra.core.Bits/charToBytes c))
                       (fn [bytes] (jclongra.core.Bits/getChar bytes 0))]

  :char java.lang.Character)


(def-bytes-converters [(fn [i]     (jclongra.core.Bits/intToBytes i))
                       (fn [bytes] (jclongra.core.Bits/getInt bytes 0))]

  :int java.lang.Integer)


(def-bytes-converters [(fn [l]     (jclongra.core.Bits/longToBytes l))
                       (fn [bytes] (jclongra.core.Bits/getLong bytes 0))]

  :long java.lang.Long)


(def-bytes-converters [(fn [f]     (jclongra.core.Bits/floatToBytes f))
                       (fn [bytes] (jclongra.core.Bits/getFloat bytes 0))]

  :float java.lang.Float)


(def-bytes-converters [(fn [d]     (jclongra.core.Bits/doubleToBytes d))
                       (fn [bytes] (jclongra.core.Bits/getDouble bytes 0))]

  :double java.lang.Double)


;; BIG INTEGERS

(def-bytes-converters
  [(fn [^BigInteger n] (.toByteArray n))
   (fn [^bytes bytes]  (BigInteger. bytes))]

  :big-integer java.math.BigInteger)


(def-bytes-converters
  [(fn [^clojure.lang.BigInt n]
     (.. n toBigInteger toByteArray))

   (fn [^bytes bytes]
     (clojure.lang.BigInt/fromBigInteger (BigInteger. bytes)))]

  :big-int clojure.lang.BigInt)


;; BIG DECIMAL

(def-bytes-converters
  [(fn [d]      (jclongra.core.Bits/bigDecimalToBytes d))
   (fn [bytes]  (jclongra.core.Bits/bytesToBigDecimal bytes))]

  :big-decimal java.math.BigDecimal)


;; RATIO

(def-bytes-converters
  [(fn [r]      (jclongra.core.Bits/ratioToBytes r))
   (fn [bytes]  (jclongra.core.Bits/bytesToRatio bytes))]

  :ratio clojure.lang.Ratio)


;; BIT SET

(def-bytes-converters [(fn [set]   (jclongra.core.Bits/bitSetToBytes set))
                       (fn [bytes] (jclongra.core.Bits/bytesToBitSet bytes))]

  :bitset java.util.BitSet)


;; DATE/TIME

(def-bytes-converters
  [(fn [^java.util.Date d] (jclongra.core.Bits/longToBytes (.getTime d)))
   (fn [bytes] (java.util.Date. (jclongra.core.Bits/getLong bytes 0)))]

  :date java.util.Date)


(def-bytes-converters
  [(fn [^java.sql.Timestamp t] (jclongra.core.Bits/longToBytes (.getTime t)))
   (fn [bytes] (java.sql.Timestamp. (jclongra.core.Bits/getLong bytes 0)))]

  :timestamp :time java.sql.Timestamp)


;; PAIRS OF INTS/LONGS

;; (def-bytes-converters
;;   [(fn [p]     (clongra.repo.RTools/intPairToBytes p))
;;    (fn [bytes] (clongra.repo.RTools/bytesToIntPair bytes))]

;;   :int-pair)


;; (def-bytes-converters
;;   [(fn [p]     (clongra.repo.RTools/longPairToBytes p))
;;    (fn [bytes] (clongra.repo.RTools/bytesToLongPair bytes))]

;;   :long-pair)
