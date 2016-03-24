;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

;; MISC STRING OPERATIONS

(declare truth)

(defn blank?
  "Returns true iff the string s either nil or blank"
  [s]
  (when (org.apache.commons.lang3.StringUtils/isBlank s)
    (truth s)))


(defn not-blank?
  "If s is not blank, it's returned, nil otherwise."
  [s]
  (when-not (blank? s)
    s))


;; TO-STRING PROTOCOL

(defprotocol WithTstr
  "A protocol that defines a Clojure unary-tstr mechanism for objects
  in an analogy to the toString method."

  (^String unary-tstr [obj] "Returns a String representation of the obj."))


(defn ^String tstr
  "Works like the clojure.core/str, but uses unary-tstr for the
  serialization of a single object into a String. By default
  transforms lazy sequences into lists for the purpose of transforming
  to string."
  ([] (str))

  ([x] (unary-tstr x))

  ([x & ys]
     (let [builder (StringBuilder. ^String (tstr x))]
       (loop [sb builder
              more ys]
         (if more
           (recur (. sb  (append (tstr (first more)))) (next more))
           (str sb))))))


(declare tstr-spaced)

(defn- limited-coll-tstr
  [coll]
  (let [plen  (if *print-length* *print-length* DEFAULT-PRINT-LENGTH)
	dots? (seq (drop plen coll))
	coll  (take plen coll)
	coll  (if dots? (concat coll [(symbol "...")]) coll)]

    (str "(" (apply tstr-spaced coll) ")")))


(extend-protocol WithTstr
  Object
  (unary-tstr [s] (.toString s))

  nil
  (unary-tstr [_] "nil")

  String
  (unary-tstr [s] s)

  clojure.lang.LazySeq
  (unary-tstr [coll] (limited-coll-tstr coll))

  clojure.lang.Cons
  (unary-tstr [coll] (limited-coll-tstr coll))

  clojure.lang.PersistentList$EmptyList
  (unary-tstr [coll] "()")

  jclongra.core.Pair
  (unary-tstr [p]
    (tstr "(" (tstr (.first p)) " . "(tstr (.second p)) ")")))


;; SOME USEFUL STRING OPERATIONS

(defmacro wft
  "Returns a macroexpanded-1 form."
  [form]
  `(macroexpand-1 '~form))


(defmacro wft-all
  "Returns a macroexpanded-all form."
  [form]
  `(clojure.walk/macroexpand-all '~form))


(defn ^String indent-string
  "Generates an indentation string of n indent-with elements
  (space character by default)."
  ([^long n] (indent-string n \space))

  ([^long n indent-with]
     (let [sb (new StringBuilder)]
       (dotimes [i n] (. sb (append indent-with)))
       (str sb))))


(defn ^String force-minimal-string-length
  [len s]
  (let [len (int len)
	s (str s)
	c (int (count s))
	diff (int (clojure.core/- len c))]

    (if (> diff (int 0)) (str (indent-string diff) s) s)))


(defn ^String unescape-unicode
  "Finds all occurences of a UnicodeEscape sequence \\ u HD HD HD
   HD in the source s and converts them into chars in the
   resulting string. HD here is
   HexDigit := {0 .. 9 | a .. f | A .. F}."
  [s]
  (jclongra.core.Unicode/unescape s))


(defn ^String pluralize
  "Returns a pluralized (inflected) form of the word."
  [word]
  (org.jactiveresource.Inflector/pluralize word))


(defn ^String singularize
  "Returns a singularized form of the word."
  [word]
  (org.jactiveresource.Inflector/singularize word))


(defn ^String dasherize
  "Replaces underscores with dashes in the string s."
  [s]
  (org.jactiveresource.Inflector/dasherize s))


(defn ^String underscorize
  "Replaces dashes with underscores in a string."
  [s]
  (org.jactiveresource.Inflector/underscorize s))


(defn ^String underscore
  "Makes an underscored form from the expression in the
  string. Changes '::' to '/' to convert namespaces to paths."
  [s]
  (org.jactiveresource.Inflector/underscore s))


(defn ^String decamelize
  "Takes a camel-cased string and a separator (space by default) and
  returns a string containing the s components separated with the
  separator."
  ([s separator]
     (let [replacement (str "$1" separator "$2")]
       (-> s
           (cstr/replace #"([A-Z]+)([A-Z][a-z])" replacement)
           (cstr/replace #"([a-z\\d])([A-Z])"    replacement))))

  ([s] (decamelize s " ")))


(defn ^String undasherize
  "Takes a dash-separated string and a separator (space by default)
  and returns a string containing the s components separated with the
  separator."
  ([s separator]
     (cstr/replace s #"-" separator))

  ([s] (undasherize s " ")))


(defn ^String deunderscorize
  "Takes an underscore-separated string and a separator (space by
  default) and returns a string containing the s components separated
  with the separator."
  ([s separator]
     (cstr/replace s #"_" separator))

  ([s] (deunderscorize s " ")))


(defn ^String tstr-interposed
  [sep & more]
  (->> more
       (map tstr)
       (interpose (tstr sep))
       (apply str)))


(defn ^String tstr-spaced
  [& more]
  (let [buf  (StringBuilder.)
        more (seq more)]
    (when more
      (.append buf (tstr (first more))))

    (doseq [obj (next more)]
      (let [s (tstr obj)]
        (when-not (.startsWith s ".")
          (.append buf " "))
        (.append buf s)))

    (.toString buf)))


;; QUALIFIED NAMES

(def
  ^{:private true :dynamic true}
  *qname-delimiter* (atom (jclongra.core.Pair/of #"\." ".")))


(declare pair)

(defn set-qname-delimiter
  [re s] (reset! *qname-delimiter* (pair re s)))


(defn ^jclongra.core.Pair qname-delimiter
  [] @*qname-delimiter*)


(defmacro with-qname-delimiter
  [re s & body]
  `(binding [*qname-delimiter* (atom (pair ~re ~s))] ~@body))


(defprotocol Qname
  (qname-seq [qname]
    "Returns the sequence of qualified name segments according to the
  current delimiter.")

  (^kongra.core.Pair qname-pair [qname]
    "Returns a pair of a qualified name and the simple name
  representing the argument.")

  (^String qname-str [qname]
    "Returns the string form of the qualified name."))


(extend-type java.util.List
  Qname
  (qname-seq  [coll] coll)

  (qname-pair [coll] (pair (butlast coll) (last coll)))

  (qname-str  [coll]
    (cstr/join (.second (qname-delimiter))
               (filter not-blank? coll))))


(extend-type jclongra.core.Pair
  Qname
  (qname-seq  [p] (concat (qname-seq (.first p)) (list (.second p))))

  (qname-pair [p] p)

  (qname-str  [p]
    (if-let [pfx (.first p)]
      (str (qname-str pfx) (.second (qname-delimiter)) (.second p))

      (.second p))))


(extend-type String
  Qname
  (qname-seq  [s] (cstr/split s (.first (qname-delimiter))))

  (qname-pair [s] (qname-pair (qname-seq s)))

  (qname-str  [s] s))


;; PERCENTAGE PRINTING

(def ^{:tag java.text.DecimalFormat}
  DEFAULT-PERCENTAGE-FORMAT (java.text.DecimalFormat. "0.00"))


(defn percentages
  "Takes a collection of numbers (series) and returns a corresponding
  collection of percentages. Beware the percentages may not total 100
  due to rounding. The default formatter (DEFAULT-PERCENTAGE-FORMAT)
  does not generate % symbol.

  e.g. (percentages [1 2 3]) → (\"16,67\" \"33,33\" \"50,00\")."
  ([^java.text.NumberFormat formatter coll]
     (let [Σ (reduce + coll)]
       (->> coll
            (map #(.format formatter (* 100 (/ % Σ)))))))

  ([coll] (percentages DEFAULT-PERCENTAGE-FORMAT coll)))


;; GENERATING CONSECUTIVE SYMBOLS

(declare N')

(defn consymbs
  ([prefix]
     (consymbs prefix 0))

  ([prefix start]
     (let [prefix (tstr prefix)]
       (map #(symbol (str prefix %)) (N' start)))))


;; ;; CONFIGURE REPL PRINTING TO USE tstr
;; ;; DEPRECATED !!!
;; ;; UNCOMMENT IF YOU REALLY WANT TO OVERRIDE THE DEFAULT BEHAVIOR !!!
;; (defn def-print-dup
;;   "Accepts the function f that is a transformer of objects into
;;   strings.  Causes every object of type to be print-dupped with
;;   (f obj)."
;;   [type f]
;;   (defmethod clojure.core/print-dup type
;;     [x writer]
;;     (when (and writer (instance? java.io.Writer writer))
;;       (. ^java.io.Writer writer (write ^String (f x)))))

;;   (defmethod clojure.core/print-method type
;;     [x writer]
;;     (when (and writer (instance? java.io.Writer writer))
;;       (. ^java.io.Writer writer (write ^String (f x))))))

;; (def-print-dup java.lang.Object tstr)
