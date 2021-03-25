(ns developer-test.core
  "Graph traversal algorithms"
  {:author "Pablo Reszczynski"}
  (:require [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map-by]]))

;; Utility functions
(defmacro for-map [bindings body]
  (let [[k v] (first body)]
    `(into {}
           (for ~bindings
             [~k ~v]))))

(defn find-first [f coll]
  (first (drop-while (complement f) coll)))

;; Specs

(s/def ::neighbor
  (s/or
   :weighted (s/cat :node keyword? :weight int?)
   :unweighted keyword?))

(s/def ::edge
  (s/cat :from keyword? :to keyword? :weight int?))

(s/def ::graph
  (s/map-of keyword?
            (s/coll-of ::neighbor
                       :into [])))

;; Question #1:
;; A simple (map first x) function over the neighbors of a node should get
;; the other neighbors keys and function the same as with the unweighted version
(defn seq-graph [d g s]
  (letfn
    [(rec-seq [explored frontier]
       (lazy-seq
        (if (empty? frontier)
          nil
          (let [v (peek frontier)
                neighbors (g v)
                neighbor-keys (if (every? seq? neighbors)
                                (map first neighbors)
                                neighbors)]
            (cons v (rec-seq
                     (into explored neighbor-keys)
                     (into (pop frontier) (remove explored neighbor-keys))))))))]
    (rec-seq #{s} (conj d s))))

(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))

(s/fdef random-pair
  :args (s/cat :nodes (s/coll-of keyword?))
  :ret ::edge)

(defn random-edge [nodes]
  "From a vector of nodes, returns a random pairing of two distinct values
   and a random weight for that edge"
  (conj (take 2 (shuffle nodes))
        (inc (rand-int 4))))

(s/fdef graph
  :args (s/cat ::edges (s/coll-of ::edge :into []))
  :ret ::graph)

(defn graph [edges]
  "Creates a adjacency-list graph from an edge-list"
  (reduce
    (fn [graph edge]
      (let [[weight from to] edge]
        (update graph from conj (list to weight))))
    {}
    edges))

(defn connected? [graph]
  "Checks if the provided graph is fully connected"
  (let [bfs (into #{} (seq-graph-bfs graph (first (keys graph))))
        nodes (into #{} (keys graph))]
     (= bfs nodes)))

(defn check-graph [n s graph]
  (let [nodes (into #{} (keys graph))
        n-edges (reduce + (map count (vals graph)))]
    (and
     (= n (count nodes))
     (= s n-edges))))

(defn weight [graph n1 n2]
  "Assuming that the graph is weighted and that n1 and n2 share an edge,
   gets the weight between n1 and n2"
  (let [edges (filter #(= (first %) n2) (get graph n1))
        weights (map second edges)]
    (apply min weights)))

(defn neighbors [graph n]
  "Gets all neighbors of n"
  (->> (get graph n)
       (map first)))



;; With this spec, we can check the that the inputs are valid and that the
;; result outputs a connected graph
(s/fdef make-graph
  :args (s/and (s/cat :size int? :sparseness int?)
               #(pos? (:size %))
               #(and (>= (:sparseness %) (dec (:size %)))
                     (<= (:sparseness %) (* (:size %)
                                            (dec (:size %))))))
  :ret ::graph
  :fn #(and (connected? (:x %))
            (check-graph (-> % :args :size)
                         (-> % :args :sparseness)
                         (:x %))))

;; Question #2
;; Generates a random directed graph with random weights
(defn make-graph
  "Generates a random graph
   where n= size
         s= sparseness (number of directed edges actually;
         from N-1 (inclusive) to N(N-1) (inclusive))"
  [n s]
  (let [nodes (map (comp keyword str) (range n))
        edges (repeatedly s #(random-edge nodes))]
    (graph edges)))

;; Question #3
;; Implementation of Dijkstra Algorithm

;; Using `clojure.core/data.priority-map` as a Min-Heap for the algorithm

(defn explore-node [graph]
  "The `next` function is loosely based on the `seq-graph` function
   to lazily traverse the graph with an updating heap that stores the explored
   nodes and costs of the paths

   The heap contains pairs of the type `(:node, [cost :prev-node])` and uses the
   cost value as the comparison key.

   The algorithm works as follow:
   Until the heap is empty,
   get the minimal cost node (v) from the heap,
   and for each neighbor node (n) of node that is not explored,
   calculate the distance as current cost + weight of path between v and n,
   add n to explored nodes,
   update the heap with the distances of the neighbor nodes
"
  (letfn
      [(extend-frontier [node unexplored cost]
         (for-map [n unexplored]
           {n [(+ cost (weight graph node n)) node]}))
       (next [explored frontier]
         (lazy-seq
          (when-let [[node [cost prev]] (peek frontier)]
            (let [path (conj (explored prev []) node)
                  explored (assoc explored node path)
                  unexplored (remove explored (into #{} (neighbors graph node)))
                  frontier (conj (pop frontier)
                                 (extend-frontier node unexplored cost))]
              (cons [node cost path]
                    (next explored frontier))))))]
    next))

(defn priority-fun [[cost1 _] [cost2 _]]
  "The priority function for our heap"
  (< cost1 cost2))

(defn make-heap [from]
  (conj
   (priority-map-by priority-fun)
   [from [0 nil]]))

(s/fdef shortest-path
  :args (s/cat :graph ::graph :from keyword? :to keyword?))

(defn shortest-path [graph from to]
  (let [heap (make-heap from)
        all-paths ((explore-node graph) {} heap)]
     (find-first (fn [[node]] (= node to)) all-paths)))

;; Question #4

(defn eccentricity [graph node]
  (let [heap (make-heap node)
        all-paths ((explore-node graph) {} heap)]
    (apply max (map second all-paths))))

(defn radius [graph]
  (let [all-eccs (map #(eccentricity graph %) (keys graph))]
    (apply min all-eccs)))

(defn diameter [graph]
  (let [all-eccs (map #(eccentricity graph %) (keys graph))]
    (apply max all-eccs)))
