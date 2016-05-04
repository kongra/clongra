;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2016-05-04

(in-ns 'clongra.core)

(deftype ^:private Kleene [name]
  Object
  (toString [_] (str name)))

(def ^Kleene KleeneTrue      (Kleene. "KleeneTrue"     ))
(def ^Kleene KleeneFalse     (Kleene. "KleeneFalse"    ))
(def ^Kleene KleeneUndefined (Kleene. "KleeneUndefined"))

(defn Kleene-not ;:- Kleene -> Kleene
  [x]
  (let [x (cast Kleene x)]
    (cond (ref= KleeneTrue  x) KleeneFalse
          (ref= KleeneFalse x) KleeneTrue
          :else                KleeneUndefined)))


(defmacro Kleene-and
  ([]  KleeneTrue)
  ([x] `(cast Kleene ~x))
  ([x & next]
   `(let [x# (cast Kleene ~x)]
      (cond (ref= KleeneFalse x#) KleeneFalse
            (ref= KleeneTrue  x#) (Kleene-and ~@next)
            :else ;; KleeneUndefined
            (if (ref= KleeneFalse (Kleene-and ~@next))
              KleeneFalse
              KleeneUndefined)))))


(defmacro Kleene-or
  ([]  KleeneFalse)
  ([x] `(cast Kleene ~x))
  ([x & next]
   `(let [x# (cast Kleene ~x)]
      (cond (ref= KleeneTrue  x#) KleeneTrue
            (ref= KleeneFalse x#) (Kleene-or ~@next)
            :else ;; KleeneUndefined
            (if (ref= KleeneTrue (Kleene-or ~@next))
              KleeneTrue
              KleeneUndefined)))))
