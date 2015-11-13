;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-20

(in-ns 'clongra.core)

;; DYNAMIC VARIABLES UTILITY

(deftype ^:private DynVar
  [value binder original?]

  clojure.lang.IDeref
  (deref [this] (deref value)))


(defmacro dyndef
  "Defines a new dynamic variable."
  ([name value]
     `(do (def ~(vary-meta name assoc :dynamic true :tag DynVar)
            (DynVar. (atom ~value) (promise) true))

          ;; register the binder
          (deliver
           (.binder ~name)

           (fn [value# body#]
             (let [new-dyn# (DynVar. (atom value#) (.binder ~name) false)]
               (binding [~name new-dyn#]
                 (body#)))))))

  ([name]
     `(dyndef ~name nil)))


(defn dynbinding*
  "Dynamically (on the run-time) executes the body (no-arg) function
  within a binding context. The argument bindings is a collection of
  [dyn value] pairs."
  [bindings body]
  (if-not (seq bindings)
    (body)

    (let [[^DynVar dyn value :as b] (first bindings)
          _ (assert (= 2 (count b)) (str "Illegal binding " b))
          binder @(.binder dyn)]

      (binder value #(dynbinding* (rest bindings) body)))))


(defmacro dynbinding
  [bindings & body]
  (assert (vector? bindings) "bindings must be a vector")
  (assert (even? (count bindings))
          "bindings must contain an even number of elements")

  (let [make-clause
        (fn [[dyn value]]
          (vector dyn
                  `(DynVar. (atom ~value) (.binder ~dyn) false)))

        clauses (->> bindings
                     (partition 2)
                     (map make-clause)
                     (apply concat)
                     vec)]

    `(binding ~clauses ~@body)))


(defn dynset!
  "Sets the value of a dynamic variable."
  [^DynVar dyn value]
  (assert (.original? dyn) "Dynamically re-bound dynvar cannot be set.")
  (reset! (.value dyn) value))


(defprotocol WithDynval
  (dynval [dyn] "Returns the value of a dynamic variable."))

(extend-protocol WithDynval
  DynVar
  (dynval [dyn] (deref (.value dyn)))

  java.lang.Object
  (dynval [dyn] dyn)

  nil
  (dynval [_] nil))


(defmacro with-dynval
  "An anaphoric macro that binds the symbol it to the current value of
  a dynamic variable of the given name."
  [name & body]
  `(let [~'it (dynval ~name)]
     ~@body))


;; (dyndef SWT-1 123)
;; (dyndef SWT-2 123)

;; (defn dyntest-01
;;   []
;;   (dynbinding* [[SWT-1 456] [SWT-2 567]]
;;                #(+ (dynval SWT-1) (dynval SWT-2))))


;; (defn dyntest-02
;;   []
;;   (dynbinding [SWT-1 456
;;                SWT-2 567]
;;     (+ (dynval SWT-1) (dynval SWT-2))))
