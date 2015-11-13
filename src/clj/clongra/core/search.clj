;; Copyright (c) Konrad Grzanek. All rights reserved.
;; Created 2014-03-21

(in-ns 'clongra.core)

;; TREE SEARCH ROUTINES INSPIRED BY PAIP , CHAPTER 6.4
;; FOR MORE SEE clongra.search

(defn tree-search
  "Searches state-spaces that have the form of trees. Starts with
  a sequence of states and performs the search according to the
  goal? predicate, generator of nodes adjacent do a given node
  and combiner responsible of adding nodes to the search
  collection of nodes."
  [states goal? adjacent combiner]
  (when (seq states)
    (let [obj (first states)]
      (if (goal? obj)
        obj

        (recur (combiner (adjacent obj) (rest states))
               goal?
               adjacent
               combiner)))))


(defn depth-first-combiner
  "The combiner for the depth-first-search."
  [new-nodes states]
  (lazy-cat new-nodes states))


(defn breadth-first-combiner
  "The combiner for the breadth-first-search."
  [new-nodes states]
  (lazy-cat states new-nodes))


(defn breadth-first-tree-levels
  "Returns a lazy collection of lazy sequences of nodes belonging
  to subsequent tree levels."
  [start adjacent]
  (->> (list start)
       (iterate #(apply concat (map adjacent %)))
       (take-while seq)))


(defn breadth-first-tree-seq
  "Returns a lazy sequence of tree nodes starting with the passed
  start node where adjacent is a function generating nodes
  adjacent to it's argument.

  Goes on infinitely unless the limiting depth specified."
  ([start adjacent]
     (apply concat (breadth-first-tree-levels start adjacent)))

  ([start adjacent depth]
     (assert (> depth 0))
     (->> (breadth-first-tree-levels start adjacent)
          (take depth)
          (reduce concat))))


(defn breadth-first-search
  "Performs a breadth first search. Goes on infinitely unless a
  maximum depth specified."
  ([start goal? adjacent]
     (first (filter goal? (breadth-first-tree-seq start adjacent))))

  ([start goal? adjacent depth]
     (first
      (filter goal?
	      (breadth-first-tree-seq start adjacent depth)))))


(defn depth-first-tree-slices
  "Returns a lazy collection of tree slices when traversing the
  tree in start node with a function adjacent that generates
  nodes adjacent to it's arguments. A slice is an information
  about the nodes to visit on current traversing stage.

  The process is infinite in depth unless the depth is specified
  explicitly."
  ([start adjacent]
     (->> (list start)
          (iterate #(lazy-cat (adjacent (first %)) (rest %)))
          (take-while seq)))

  ([start adjacent depth]
     (assert (> depth 0))

     (let [node       #(pair %1 %2)
	   node-depth #(pair-first %)
	   node-value #(pair-second %)]
       (map #(map node-value %)
	    (take-while seq
			(iterate
			 #(lazy-cat
			   (let [n  (first %) nd (node-depth n)]
			     (when (< nd (dec depth))
			       (map (partial node (inc nd))
				    (adjacent (node-value n)))))

			   (rest %))

			 (list (node 0 start))))))))


(defn depth-first-tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a
  depth-first walk.  Starts with a node called start. The
  function adjacent generates adjacent nodes (children) for a
  passed node."
  ([start adjacent]
     (map first (depth-first-tree-slices start adjacent)))

  ([start adjacent depth]
     (map first (depth-first-tree-slices start adjacent depth))))


(defn depth-first-search
  ([start goal? adjacent]
     (first (filter goal? (depth-first-tree-seq start adjacent))))

  ([start goal? adjacent depth]
     (first (filter goal?
		    (depth-first-tree-seq start adjacent depth)))))


;; BINARY SEARCH

(defn binary-search
  "A binary search, ∞ aware. Assumes coll is a finite sorted
  collection. Returns the element if one found, not-found
  otherwise. The comparator is (fn [key element] ...).

  Works SOLELY on long indices, co expects the number of elements of
  coll within long range. WRT the RAM capacities this is going to be
  asserted now and in foresable future."
  ([coll key comptor not-found]
     (let [n (len coll)]
       (when (+∞? n)
         (terror IllegalArgumentException "Infinite colls not allowed."))

       (loop [l 0
              u (unchecked-dec (long n))]

         (if (< u l)
           not-found

           (let [i (jclongra.core.Numbers/uncheckedLongMid l u)
                 element (enth coll i)
                 result (long (comptor key element))]

             (cond (< result 0) (recur l (unchecked-dec i))
                   (> result 0) (recur (unchecked-inc i) u)
                   :else element))))))

  ([coll key comptor]
     (binary-search coll key comptor nil))

  ([coll key]
     (binary-search coll key clojure.core/compare nil)))


(defn- binary-search-mid'
  [l u]
  (+' l (integer (/ (-' u l) 2))))


(defn binary-search'
  "Works like binary-search, but accepts colls that have len(gth)s in
  ranges wider than Long."
  ([coll key comptor not-found]
     (let [n (len coll)]
       (when (+∞? n)
         (terror IllegalArgumentException "Infinite colls not allowed."))

       (loop [l (Long/valueOf 0)
              u (dec n)]

         (if (< u l)
           not-found

           (let [i (binary-search-mid' l u)
                 element (enth coll i)
                 result (long (comptor key element))]

             (cond (< result 0) (recur l (dec i))
                   (> result 0) (recur (inc i) u)
                   :else element))))))

  ([coll key comptor]
     (binary-search' coll key comptor nil))

  ([coll key]
     (binary-search' coll key clojure.core/compare nil)))
