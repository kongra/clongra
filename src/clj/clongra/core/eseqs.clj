;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-04-01

(in-ns 'clongra.core)

;; RECOGNITION

(defn eseq?
  [coll]
  (instance? jclongra.core.eseq.IEnhancedSeq coll))


;; ENHANCED LEN(GTH)

(defn len
  [coll]
  (if (eseq? coll)
    (.len ^jclongra.core.eseq.IEnhancedSeq coll)

    (count coll)))


(defn with-len
  [len coll]
  (jclongra.core.eseq.ESeq/withLen len coll))


(defn with-len-value
  [value coll]
  (with-len (return value) coll))


(defmacro with-delayed-len
  [len-expr coll]
  `(let [d# (delay ~len-expr)]
     (with-len (return @d#) ~coll)))


(defn with-len-like
  [origin coll]
  (with-len-value (len origin) coll))


;; ENHANCED NTH

(defn enth
  [coll n]
  (if (eseq? coll)
    (.enth ^jclongra.core.eseq.IEnhancedSeq coll n)

    (nth coll n)))


(defn with-enth
  [nth coll]
  (jclongra.core.eseq.ESeq/withEnth nth coll))


;; INFINITY/FINITY

(defn infinite?
  [coll]
  (+∞? (len coll)))


(defn finite?
  [coll]
  (not (infinite? coll)))


(defn infinite
  [coll]
  (with-len-value +∞ coll))


;; SOME PREDEFINED ESEQ OPERATORS

(declare binary-search)

(defn cat-eseq
  "Returns an enhaced collection being the result of concatenating the
  passed colls. It is assumed the number of colls is finite."
  [& colls]
  (let [;; prepare colls and lens (delayed)
        len-colls-bundle
        (delay
          (let [lens (map len colls)
                ;; take only the colls up to the first with len=+∞ (inclusively)
                ∞-idx (find-idx' +∞? lens)
                colls (if ∞-idx (take (inc' ∞-idx) colls) colls)
                lens  (if ∞-idx (take (inc' ∞-idx) lens)  lens)]
            (pair lens colls)))

        lens  #(pair-first  @len-colls-bundle)
        colls #(pair-second @len-colls-bundle)

        ;; prepare enth (with delayed realization)
        intervs-intermap-bundle
        (delay
          (let [intervs (->> (lens)
                             (cons 0)
                             (apply cummulative-intervs')
                             vec) ;; essential wrt performance
                intermap (zipmap intervs (colls))]
            (pair intervs intermap)))

        intervs  #(pair-first  @intervs-intermap-bundle)
        intermap #(pair-second @intervs-intermap-bundle)

        enth-impl
        (fn [n]
          ;; 1. select proper interval
          (let [intv (binary-search (intervs) n
                                    #(interv-compare (lv "[, )") %2 %1))]
            (when-not intv (terror IndexOutOfBoundsException n))

            (let [ ;; 2. select collection
                  coll ((intermap) intv)
                  ;; 3. calculate the index in the collection
                  i (- n (:start intv))]
              ;; 4. get the result wrapped with the transformation
              (enth coll i))))]

    (->> (apply concat (colls))
         (with-enth enth-impl)
         (with-delayed-len (reduce ∞+' (lens))))))
