;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

;; GARBAGE COLLECTION AND HEAP MONITORING

(defn room
  "Prints the current JVM memory status (in MB)."
  []
  (io!
    (let [free-memory  (.. Runtime getRuntime freeMemory)
          total-memory (.. Runtime getRuntime totalMemory)
          max-memory   (.. Runtime getRuntime maxMemory)
          used-memory  (- total-memory free-memory)

          scale (fn [arg] (double (/ arg (* 1024 1024))))]

      (printf "Used memory  : %f MB\n" (scale used-memory))
      (printf "Free memory  : %f MB\n" (scale free-memory))
      (printf "Total memory : %f MB\n" (scale total-memory))
      (printf "Max memory   : %f MB\n" (scale max-memory))

      nil)))


(defn gc
  "Runs the JVM garbage collector. When not silent, runs room."
  ([]
     (gc :verbose))

  ([silent?]
     (io!
       (System/gc)
       (when-not-flag silent?
         (room)))))


;; THE PROTOCOL OF OBJECTS CONVERTIBLE TO java.io.File

(defprotocol FileConvertible
  (^java.io.File to-file [obj]
    "Returns the obj converted to java.io.File."))


(extend-protocol FileConvertible
  java.io.File
  (to-file [f] f)

  java.lang.String
  (to-file [s] (java.io.File. s)))


(defn ^java.io.File ensuring-parent-directory!
  "If the file does not exist, creates the required directory
  structure based on the parent pathname. Returns the argument."
  [^java.io.File f]
  (io!
    (when-not (.exists f)
      (when-let [parent (.getParentFile f)]
        (when-not (.exists parent)
          (.mkdirs parent)))))
  f)


(defn ^java.io.File ensuring-file-exists!
  "If the file f does not exist creates the file ensuring the required
  directory structure. Returns the argument."
  [^java.io.File f]
  (io!
    (when-not (.exists f)
      (when-let [parent (.getParentFile f)]
        (when-not (.exists parent)
          (.mkdirs parent)))

      (.createNewFile f)))

  f)


;; EXECUTING SYSTEM PROCESSES (COMMANDS)

(defn exec!
  "Wrapper around Runtime.exec. Executes the specified command
  and waits for it to finish. Returns the exit code. The envp
  parameter is a sequence of strings 'name=value', or null where
  name is the environment variable name.

  out-f and err-f are unary functions that take a consecutive
  output and error lines."

  ([cmd dir envp out-f err-f]
     (io!
       (let [cmd (if (sequential? cmd)
                   (into-array String (map tstr cmd))
                   (tstr cmd))

             dir  (when dir (to-file (tstr dir)))
             envp (when (seq envp) (into-array String (map tstr envp)))]

         (jclongra.core.Tools/exec cmd dir envp out-f err-f))))

  ([cmd dir out-f err-f]
     (exec! cmd dir nil out-f err-f))

  ([cmd dir out-f]
     (exec! cmd dir nil out-f nil))

  ([cmd out-f]
     (exec! cmd nil nil out-f nil))

  ([cmd]
     (exec! cmd nil nil nil nil)))


(defn exec-nowait!
  "Works like exec, but does not handle output and errors from
  the process, neither does wait for it to end."
  ([cmd dir envp]
     (io!
       (let [cmd (if (sequential? cmd)
                   (into-array String (map tstr cmd))
                   (tstr cmd))

             dir  (when dir (to-file (tstr dir)))
             envp (when (seq envp) (into-array String (map tstr envp)))]

         (jclongra.core.Tools/execNowait (tstr cmd) dir envp))))

  ([cmd dir]
     (exec-nowait! cmd dir nil))

  ([cmd]
     (exec-nowait! cmd nil nil)))


;; MISC.

(defn system-hashcode
  [obj]
  (System/identityHashCode obj))


(defmacro swank-break
  "Breaks the execution and goes down to the SWANK diagnostic REPL."
  []
  `(swank.core/break))


(defmacro synchronized
  "Executes the body inside the synchronization block set on the
  monitor."
  [monitor & body]
  `(clongra.core.Synchronized/invoke ~monitor (fn [] ~@body)))


;; FILESYSTEM

(defn files-in
  "Returns a lazy sequence of files in the specified
  location. Goes with breadth-first-tree-seq by default, but
  allows to pass an optional strategy where strategy
  e.g. depth-first-tree-seq."
  ([dir]
     (files-in dir breadth-first-tree-seq))

  ([dir strategy]
     (io!
       (strategy (to-file dir)
                 (fn [^java.io.File d]
                   (when (.isDirectory d)
                     (seq (.listFiles d))))))))


(defn zip-entries-in
  "Returns a lazy sequence of ZipEntry objects in the given ZIP
  file."
  [file]
  (io!
    (let [file (cond (instance? java.util.zip.ZipFile file)
                     file

                     (instance? java.io.File file)
                     (new java.util.zip.ZipFile ^java.io.File file)

                     :else
                     (new java.util.zip.ZipFile
                          (new java.io.File (tstr file))))]

      (tseq (.entries ^java.util.zip.ZipFile file)))))


(defn file-extension
  "Returns the extension (with dot, e.g. .java) of the passed
  file."
  [f]
  (let [f (tstr f)
	dot-idx (. f (lastIndexOf (int \.)))]
    (when (> dot-idx -1)
      (. f (substring dot-idx)))))


(defn with-extensions
  "Creates a filtering predicate for files having the extension
  out of a given set of exts."
  [ext & exts]
  (let [exts (conj (set exts) ext)]
    (fn [f]
      (let [name (cond (instance? java.io.File f)
		       (.getPath ^java.io.File f)

		       (instance? java.util.zip.ZipEntry f)
		       (.getName ^java.util.zip.ZipEntry f)

		       :else
		       (terror "The argument " f
			       " is not a File nor a ZipEntry."))]

	(member? exts (file-extension name))))))


(defn rmdir!
  "Recursively deletes a directory and all of it's contents."
  [^java.io.File dir]
  (io!
    (when dir
      (if-not (.exists dir)
        true

        (when (.isDirectory dir)
          (doseq [^String l (.list dir)]
            (let [entry (java.io.File. dir l)]
              (if (.isDirectory entry)
                (rmdir! entry)

                (.delete entry))))

          (.delete dir))))))


;; STM

(defn transaction-running?
  "Answers the question of a presence of a (dosync ...) locking
  transaction."
  []
  (clojure.lang.LockingTransaction/isRunning))


;; TERMINAL COLORS

(def
  ^{:doc "Returns a terminal color marker for the specified color
 symbol.  Thanks to:
 http://ubuntuforums.org/showthread.php?t=341144"
    :arglists '([color])}

  termcolor-marker '{REDB     "\033[1;41m"
                     REDF     "\033[31m"
                     GREENB   "\033[1;42m"
                     GREENF   "\033[1;32m"
                     YELLOWB  "\033[1;43m"
                     YELLOWF  "\033[1;33m"
                     BLUEB    "\033[1;44m"
                     BLUEF    "\033[1;34m"
                     MAGENTAB "\033[1;45m"
                     MAGENTAF "\033[1;35m"
                     CYANB    "\033[1;46m"
                     CYANF    "\033[1;36m"
                     WHITEB   "\033[1;47m"
                     WHITEF   "\033[1;37m"
                     RESET    "\033[0m"})


(defn with-termcolor
  "Returns the string s wrapped with the terminal color marker
  for the given color."
  [color s]
  (tstr (termcolor-marker color) s (termcolor-marker 'RESET)))


;; BEAN TREE PRINTING

(defn bean-tree
  "Displays the tree representation of the given object. Uses the
  following options:
  * max-depth         - see clongra.pprint/tree
  * printer           - see clongra.pprint/tree
  * printers          - see clongra.pprint/tree
  * fold-pred         - when true on obj the obj will not be expanded
  * properties-reader - reads the collection of properties of the given object
  * obj-reader        - (fn [property obj] ...) reads the property value of
                                                the obj
  * property-to-str   - converts the property to a String
  * obj-to-str        - converts the obj to a String
  * obj-type-to-str   - converts the type of the obj to a String."
  {:arglists '([obj] [obj {:keys [max-depth
                                  printer
                                  printers
                                  fold-pred
                                  properties-reader
                                  obj-reader
                                  property-to-str
                                  obj-to-str
                                  obj-type-to-str] :as options}])}
  ([obj options]
     (validate-legal-keys [:max-depth
                           :printer
                           :printers
                           :fold-pred
                           :properties-reader
                           :obj-reader
                           :property-to-str
                           :obj-to-str
                           :obj-type-to-str] options)

     (let [f (clojure.lang.RT/var "clongra.reflect" "bean-tree")]
       (f obj options)))

  ([obj] (bean-tree obj nil)))


(defn detailed-bean-tree
  "Works like bean-tree but displays the internalst of some objects
  complex in nature, e.g. collections."
  {:arglists '([obj] [obj {:keys [max-depth
                                  printer
                                  printers
                                  fold-pred
                                  properties-reader
                                  obj-reader
                                  property-to-str
                                  obj-to-str
                                  obj-type-to-str] :as options}])}
  ([obj options]
     (validate-legal-keys [:max-depth
                           :printer
                           :printers
                           :fold-pred
                           :properties-reader
                           :obj-reader
                           :property-to-str
                           :obj-to-str
                           :obj-type-to-str] options)

     (let [f (clojure.lang.RT/var "clongra.reflect" "detailed-bean-tree")]
       (f obj options)))

  ([obj] (detailed-bean-tree obj nil)))
