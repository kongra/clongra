;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2015-12-01

(in-ns 'clongra.core)

(def ^:private PRINT-TREE-INDENT       "│   ")
(def ^:private PRINT-TREE-EMPTYINDENT  "    ")
(def ^:private PRINT-TREE-FORCHILD     "├── ")
(def ^:private PRINT-TREE-FORLASTCHILD "└── ")
(def ^:private PRINT-TREE-EOL          "\n"   )
(def ^:private PRINT-TREE-EMPTY        ""    )

(defn ^:private print-tree-indent-symbol
  [is-empty]
  (if is-empty PRINT-TREE-EMPTYINDENT PRINT-TREE-INDENT))


(defn ^:private print-tree-genindent
  [[is-last & last-child-infos]]
  (let [suffix (if is-last PRINT-TREE-FORLASTCHILD PRINT-TREE-FORCHILD)
        prefix (->> last-child-infos
                    butlast
                    reverse
                    (map print-tree-indent-symbol)
                    (apply str))]

    (str prefix suffix)))


(defn ^:private print-tree-impl
  [node adjacent show depth level last-child-infos is-first]
  (let [s    (show node)
        pfx  (if is-first PRINT-TREE-EMPTY PRINT-TREE-EOL)
        repr (if (zero? level)
               (str pfx s)
               (str pfx (print-tree-genindent last-child-infos) s))]

    (print repr)

    (when-not (= level depth)
      (let [next-level (inc level)
            children   (adjacent node)]
        (doseq [[child is-last] (map pair children (mark-last children))]
          (print-tree-impl child adjacent show depth next-level
                           (cons is-last last-child-infos) false))))))


(defn print-tree
  "Prints a tree using a textual representation like in UNIX tree command.
  adjacent : node -> [node]
  show     : node -> String"
  ([node adjacent]
   (print-tree node adjacent str))

  ([node adjacent show]
   (print-tree node adjacent show Long/MAX_VALUE))

  ([node adjacent show depth]
   (let [depth (if (< depth 1) 1 depth)]
     (print-tree-impl node adjacent show depth 0 '(true) true))))
