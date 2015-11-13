;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-20

(in-ns 'clongra.core)

(defmacro ignoring-exceptions
  [exceptions & body]
  `(try ~@body
        ~@(map (fn [ex] `(catch ~ex ~'_ nil)) exceptions)))


(defmacro ignoring-all-exceptions
  "Executes the body of expressions ignoring any throwables."
  [& body]
  `(ignoring-exceptions [Throwable] ~@body))


(defmacro try-times
  "Executes the body. If an exception of the given class is thrown, will
  retry. At most n retries are done. If still some exception is thrown
  it is bubbled upwards in the call chain.

  Source: http://stackoverflow.com/questions/1879885/
                 clojure-how-to-to-recur-upon-exception"
  [exception-class n & body]
  `(loop [n# (dec (long ~n))]
     (if-let [result# (try [(do ~@body)]
                           (catch ~exception-class e#
                             (when (zero? n#)
                               (throw e#))))]
       (result# 0)

       (recur (dec n#)))))


;; STACK-TRACES

(defn ^String stack-trace-string
  [^Throwable t]
  (let [o (java.io.ByteArrayOutputStream.)]
    (.printStackTrace t (java.io.PrintStream. o))
    (.toString o)))


(defn stack-trace
  [^Throwable t] (seq (.getStackTrace t)))


(defmacro current-stack-trace
  "Returns a sequence of StackTraceElements representing the
  current stack trace."
  []
  `(seq (. (Exception.) getStackTrace)))


(defmacro current-stack-frame
  "Returns nth (0th by default) StackTraceElement of the current
  stack trace. "
  ([n] `(nth (current-stack-trace) ~n))

  ([] `(current-stack-frame 0)))


(declare with-termcolor)

(defn prst
  "Prints the throwable stack trace using the passed printing
  function. When printing the dependencies (causes) does not
  filter out the repeating stack trace elements, like the
  t.printStackTrace() does.

  By default System.err.println-s the current *e value."

  ([printfn ^Throwable t]
     (printfn (with-termcolor 'REDB (tstr t)))
     (doseq [^String st (map tstr (stack-trace t))]
       (let [color (if (.contains st "clongra.") 'REDB ' RESET)]
	 (printfn (tstr "\t" (with-termcolor color
			      (tstr "at " st))))))

     (when-let [cause (.getCause t)]
       (printfn "Caused by: ")
       (recur printfn cause)))

  ([printfn] (when *e (prst printfn *e)))

  ([] (prst #(.. System err (println %)))))
