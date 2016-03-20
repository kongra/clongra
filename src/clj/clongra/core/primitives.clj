;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-20

(in-ns 'clongra.core)

(defmacro boolean-not [b] `(jclongra.core.Primitives/booleanNot ~b))

(defn make-longs ;:- long -> long[]
  {:inline (fn [size] `(jclongra.core.Primitives/makeLongs ~size))}
  [^long size]
  (jclongra.core.Primitives/makeLongs size))


(defn make-doubles ;:- long -> double[]
  {:inline (fn [size] `(jclongra.core.Primitives/makeDoubles ~size))}
  [^long size]
  (jclongra.core.Primitives/makeDoubles size))
