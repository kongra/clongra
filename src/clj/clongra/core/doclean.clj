;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

;; CLEANUP CONTEXT

(dyndef *doclean* nil)

(defn ^jclongra.core.Doclean doclean-create
  [] (jclongra.core.Doclean.))


(defn doclean-close!
  [^jclongra.core.Doclean d]
  (.close d))


(defmacro with-doclean
  [d & body]
  `(binding [*doclean* ~d] ~@body))


(defn ^jclongra.core.Doclean doclean-assert
  []
  (with-dynval *doclean*
    (assert it "No doclean context.")
    it))


(defn doclean-register!
  "Registeres a function as a closeable in the current cleanup
  context."
  ([^jclongra.core.Doclean doclean f]
     (.register doclean (reify java.io.Closeable
                          (close [this] (f)))))

  ([f]
     (doclean-register! (doclean-assert) f)))


(defmacro docleaned
  "Asserts a doclean, evaluates the expression and registeres it's
  value within the doclean. Finally returns the value. The cleanup is
  simply calling .close on the value."
  [expr]
  `(let [doclean# (doclean-assert)
         value#   ~expr]

     (doclean-register! doclean# (fn [] (.close value#)))
     value#))


(defmacro docleaned-with
  "An anaphoric macro. Asserts a doclean, evaluates the expression and
  binds it's value to a symbol it. Registeres it in doclean and
  returns it. The first argument is a cleaning expression - one is
  allowed to make references to it within the expression."
  [cleanup-expr expr]
  (let [it 'it]
    `(let [doclean# (doclean-assert)
           ~it       ~expr]

       (doclean-register! doclean# (fn [] ~cleanup-expr))
       ~it)))


(defmacro doclean
  "Executes the body of expressions in a new clongra.core.Doclean
  cleanup context.

  WARNING. It's highly recommended to wrap all lazy expressions
  in a (doall ...), force Delays etc."
  [& body]
  `(let [doclean# (doclean-create)]
     (with-doclean doclean#
       (with-open [dc# doclean#]
         ~@body))))
