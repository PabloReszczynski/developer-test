(ns developer-test.core
  "Graph traversal algorithms"
  {:author "Pablo Reszczynski"})

(defn seq-graph [d g s]
  (letfn
      [(rec-dfs [explored frontier]
         (lazy-seq
          (if (empty? frontier)
            nil
            (let [v (peek frontier)
                  neighbors (g v)]
              (cons v (rec-dfs
                       (into explored neighbors)
                       (into (pop frontier) (remove explored neighbors))))))))]
    (rec-dfs #{s} (conj d s))))

(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))
