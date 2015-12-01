(defproject clongra "0.1.0-SNAPSHOT"
  :description      "Core Clojure codebase"
  :url              "https://github.com/kongra/clongra"
  :license {:name   "Eclipse Public License"
            :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure                 "1.7.0"]
                 [org.clojure/math.combinatorics      "0.1.1"]
                 [org.clojure/math.numeric-tower      "0.0.4"]
                 [org.apache.commons/commons-lang3    "3.4"  ]
                 [org.apache.logging.log4j/log4j-core "2.4.1"]
                 [joda-time/joda-time                 "2.9.1"]

                 ;; FOR DEV. PURPOSES ONLY
                 [criterium "0.4.3"]
                 ]

  ;; FOR DEV. PURPOSES ONLY
  :plugins       [[cider/cider-nrepl "0.9.1"]]

  :main          clongra.core
  :aot           :all

  :hooks         []
  :disable-deps-clean true

  :source-paths   ["src/clj"]

  :resource-paths ["lib/jclongra.jar" "lib/flextao-inflector.jar"]

  :global-vars    {*warn-on-reflection* true
                   *print-length*       500})
