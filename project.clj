(defproject clongra "0.1.0-SNAPSHOT"
  :description      "Core Clojure codebase"
  :url              "https://github.com/kongra/clongra"
  :license {:name   "Eclipse Public License"
            :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure              "1.7.0"]
                 [criterium                        "0.4.3"]
                 [org.apache.commons/commons-lang3 "3.4"  ]]

  :main          clongra.core
  :aot           :all

  :hooks         []
  :disable-deps-clean true

  :plugins       [[cider/cider-nrepl                "0.9.1"]]

  :source-paths   ["src/clj"]

  :resource-paths ["lib/jclongra.jar"]

  :global-vars    {*warn-on-reflection* true
                   *print-length*       500}

  :jvm-opts       ["-server"

                   "-d64"

                   "-Xshare:off"
                   "-XX:+AggressiveOpts"
                   "-XX:+DoEscapeAnalysis"
                   "-XX:+UseCompressedOops"
                   ;; "-XX:+UseNUMA" ;; to check: numactl --hardware

                   ;; HEAP SETTINGS
                   "-Xms256m"
                   "-Xmx256m"

                   ;; PARALLEL GC SETTINGS
                   "-XX:+UseParallelGC"
                   "-XX:+UseParallelOldGC"
                   "-XX:NewSize=100m"
                   "-XX:MaxNewSize=100m"
                   "-XX:-UseAdaptiveSizePolicy"
                   "-XX:SurvivorRatio=6"

                   ;; GC DEBUG
                   ;; "-verbose:gc"
                   "-XX:+PrintGCDetails"
                   "-XX:+PrintGCTimeStamps"
                   ;; "-Xloggc:gc.log"
                   ;; "-XX:+PrintTenuringDistribution"

                   ;; JCONSOLE DEBUG
                   ;; "-Xdebug"
                   ;; "-Xrunjdwp:transport=dt_socket,address=8021,server=y,suspend=n"
                   ])
